{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImageAttribute.html>
module Network.AWS.EC2.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , dia1DryRun
    , dia1ImageId
    , dia1Attribute

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response constructor
    , describeImageAttributeResponse
    -- ** Response lenses
    , diarLaunchPermissions
    , diarRAMDiskId
    , diarKernelId
    , diarSRIOVNetSupport
    , diarImageId
    , diarProductCodes
    , diarBlockDeviceMappings
    , diarDescription
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImageAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dia1DryRun'
--
-- * 'dia1ImageId'
--
-- * 'dia1Attribute'
data DescribeImageAttribute = DescribeImageAttribute'{_dia1DryRun :: Maybe Bool, _dia1ImageId :: Text, _dia1Attribute :: ImageAttributeName} deriving (Eq, Read, Show)

-- | 'DescribeImageAttribute' smart constructor.
describeImageAttribute :: Text -> ImageAttributeName -> DescribeImageAttribute
describeImageAttribute pImageId pAttribute = DescribeImageAttribute'{_dia1DryRun = Nothing, _dia1ImageId = pImageId, _dia1Attribute = pAttribute};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dia1DryRun :: Lens' DescribeImageAttribute (Maybe Bool)
dia1DryRun = lens _dia1DryRun (\ s a -> s{_dia1DryRun = a});

-- | The ID of the AMI.
dia1ImageId :: Lens' DescribeImageAttribute Text
dia1ImageId = lens _dia1ImageId (\ s a -> s{_dia1ImageId = a});

-- | The AMI attribute.
--
-- __Note__: Depending on your account privileges, the @blockDeviceMapping@
-- attribute may return a @Client.AuthFailure@ error. If this happens, use
-- DescribeImages to get information about the block device mapping for the
-- AMI.
dia1Attribute :: Lens' DescribeImageAttribute ImageAttributeName
dia1Attribute = lens _dia1Attribute (\ s a -> s{_dia1Attribute = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeImageAttribute where
        type Sv DescribeImageAttribute = EC2
        type Rs DescribeImageAttribute =
             DescribeImageAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImageAttributeResponse' <$>
                   (may (parseXMLList "item") x) <*> (x .@? "ramdisk")
                     <*> (x .@? "kernel")
                     <*> (x .@? "sriovNetSupport")
                     <*> (x .@? "imageId")
                     <*> (may (parseXMLList "item") x)
                     <*> (may (parseXMLList "item") x)
                     <*> (x .@? "description"))

instance ToHeaders DescribeImageAttribute where
        toHeaders = const mempty

instance ToPath DescribeImageAttribute where
        toPath = const "/"

instance ToQuery DescribeImageAttribute where
        toQuery DescribeImageAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImageAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dia1DryRun, "ImageId" =: _dia1ImageId,
               "Attribute" =: _dia1Attribute]

-- | /See:/ 'describeImageAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diarLaunchPermissions'
--
-- * 'diarRAMDiskId'
--
-- * 'diarKernelId'
--
-- * 'diarSRIOVNetSupport'
--
-- * 'diarImageId'
--
-- * 'diarProductCodes'
--
-- * 'diarBlockDeviceMappings'
--
-- * 'diarDescription'
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'{_diarLaunchPermissions :: Maybe [LaunchPermission], _diarRAMDiskId :: Maybe AttributeValue, _diarKernelId :: Maybe AttributeValue, _diarSRIOVNetSupport :: Maybe AttributeValue, _diarImageId :: Maybe Text, _diarProductCodes :: Maybe [ProductCode], _diarBlockDeviceMappings :: Maybe [BlockDeviceMapping], _diarDescription :: Maybe AttributeValue} deriving (Eq, Read, Show)

-- | 'DescribeImageAttributeResponse' smart constructor.
describeImageAttributeResponse :: DescribeImageAttributeResponse
describeImageAttributeResponse = DescribeImageAttributeResponse'{_diarLaunchPermissions = Nothing, _diarRAMDiskId = Nothing, _diarKernelId = Nothing, _diarSRIOVNetSupport = Nothing, _diarImageId = Nothing, _diarProductCodes = Nothing, _diarBlockDeviceMappings = Nothing, _diarDescription = Nothing};

-- | One or more launch permissions.
diarLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarLaunchPermissions = lens _diarLaunchPermissions (\ s a -> s{_diarLaunchPermissions = a}) . _Default;

-- | The RAM disk ID.
diarRAMDiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarRAMDiskId = lens _diarRAMDiskId (\ s a -> s{_diarRAMDiskId = a});

-- | The kernel ID.
diarKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarKernelId = lens _diarKernelId (\ s a -> s{_diarKernelId = a});

-- | FIXME: Undocumented member.
diarSRIOVNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarSRIOVNetSupport = lens _diarSRIOVNetSupport (\ s a -> s{_diarSRIOVNetSupport = a});

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarImageId = lens _diarImageId (\ s a -> s{_diarImageId = a});

-- | One or more product codes.
diarProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarProductCodes = lens _diarProductCodes (\ s a -> s{_diarProductCodes = a}) . _Default;

-- | One or more block device mapping entries.
diarBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarBlockDeviceMappings = lens _diarBlockDeviceMappings (\ s a -> s{_diarBlockDeviceMappings = a}) . _Default;

-- | A description for the AMI.
diarDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarDescription = lens _diarDescription (\ s a -> s{_diarDescription = a});
