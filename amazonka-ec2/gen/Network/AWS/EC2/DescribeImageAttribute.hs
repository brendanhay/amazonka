{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified AMI. You can specify
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
    , diaarqDryRun
    , diaarqImageId
    , diaarqAttribute

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response constructor
    , describeImageAttributeResponse
    -- ** Response lenses
    , diaarsLaunchPermissions
    , diaarsRAMDiskId
    , diaarsKernelId
    , diaarsSRIOVNetSupport
    , diaarsImageId
    , diaarsProductCodes
    , diaarsBlockDeviceMappings
    , diaarsDescription
    , diaarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImageAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaarqDryRun'
--
-- * 'diaarqImageId'
--
-- * 'diaarqAttribute'
data DescribeImageAttribute = DescribeImageAttribute'
    { _diaarqDryRun    :: !(Maybe Bool)
    , _diaarqImageId   :: !Text
    , _diaarqAttribute :: !ImageAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImageAttribute' smart constructor.
describeImageAttribute :: Text -> ImageAttributeName -> DescribeImageAttribute
describeImageAttribute pImageId pAttribute =
    DescribeImageAttribute'
    { _diaarqDryRun = Nothing
    , _diaarqImageId = pImageId
    , _diaarqAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diaarqDryRun :: Lens' DescribeImageAttribute (Maybe Bool)
diaarqDryRun = lens _diaarqDryRun (\ s a -> s{_diaarqDryRun = a});

-- | The ID of the AMI.
diaarqImageId :: Lens' DescribeImageAttribute Text
diaarqImageId = lens _diaarqImageId (\ s a -> s{_diaarqImageId = a});

-- | The AMI attribute.
--
-- __Note__: Depending on your account privileges, the @blockDeviceMapping@
-- attribute may return a @Client.AuthFailure@ error. If this happens, use
-- DescribeImages to get information about the block device mapping for the
-- AMI.
diaarqAttribute :: Lens' DescribeImageAttribute ImageAttributeName
diaarqAttribute = lens _diaarqAttribute (\ s a -> s{_diaarqAttribute = a});

instance AWSRequest DescribeImageAttribute where
        type Sv DescribeImageAttribute = EC2
        type Rs DescribeImageAttribute =
             DescribeImageAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImageAttributeResponse' <$>
                   (x .@? "launchPermission" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "ramdisk")
                     <*> (x .@? "kernel")
                     <*> (x .@? "sriovNetSupport")
                     <*> (x .@? "imageId")
                     <*>
                     (x .@? "productCodes" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*>
                     (x .@? "blockDeviceMapping" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

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
               "DryRun" =: _diaarqDryRun,
               "ImageId" =: _diaarqImageId,
               "Attribute" =: _diaarqAttribute]

-- | Describes an image attribute.
--
-- /See:/ 'describeImageAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaarsLaunchPermissions'
--
-- * 'diaarsRAMDiskId'
--
-- * 'diaarsKernelId'
--
-- * 'diaarsSRIOVNetSupport'
--
-- * 'diaarsImageId'
--
-- * 'diaarsProductCodes'
--
-- * 'diaarsBlockDeviceMappings'
--
-- * 'diaarsDescription'
--
-- * 'diaarsStatus'
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
    { _diaarsLaunchPermissions   :: !(Maybe [LaunchPermission])
    , _diaarsRAMDiskId           :: !(Maybe AttributeValue)
    , _diaarsKernelId            :: !(Maybe AttributeValue)
    , _diaarsSRIOVNetSupport     :: !(Maybe AttributeValue)
    , _diaarsImageId             :: !(Maybe Text)
    , _diaarsProductCodes        :: !(Maybe [ProductCode])
    , _diaarsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _diaarsDescription         :: !(Maybe AttributeValue)
    , _diaarsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImageAttributeResponse' smart constructor.
describeImageAttributeResponse :: Int -> DescribeImageAttributeResponse
describeImageAttributeResponse pStatus =
    DescribeImageAttributeResponse'
    { _diaarsLaunchPermissions = Nothing
    , _diaarsRAMDiskId = Nothing
    , _diaarsKernelId = Nothing
    , _diaarsSRIOVNetSupport = Nothing
    , _diaarsImageId = Nothing
    , _diaarsProductCodes = Nothing
    , _diaarsBlockDeviceMappings = Nothing
    , _diaarsDescription = Nothing
    , _diaarsStatus = pStatus
    }

-- | One or more launch permissions.
diaarsLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diaarsLaunchPermissions = lens _diaarsLaunchPermissions (\ s a -> s{_diaarsLaunchPermissions = a}) . _Default;

-- | The RAM disk ID.
diaarsRAMDiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diaarsRAMDiskId = lens _diaarsRAMDiskId (\ s a -> s{_diaarsRAMDiskId = a});

-- | The kernel ID.
diaarsKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diaarsKernelId = lens _diaarsKernelId (\ s a -> s{_diaarsKernelId = a});

-- | FIXME: Undocumented member.
diaarsSRIOVNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diaarsSRIOVNetSupport = lens _diaarsSRIOVNetSupport (\ s a -> s{_diaarsSRIOVNetSupport = a});

-- | The ID of the AMI.
diaarsImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diaarsImageId = lens _diaarsImageId (\ s a -> s{_diaarsImageId = a});

-- | One or more product codes.
diaarsProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diaarsProductCodes = lens _diaarsProductCodes (\ s a -> s{_diaarsProductCodes = a}) . _Default;

-- | One or more block device mapping entries.
diaarsBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diaarsBlockDeviceMappings = lens _diaarsBlockDeviceMappings (\ s a -> s{_diaarsBlockDeviceMappings = a}) . _Default;

-- | A description for the AMI.
diaarsDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diaarsDescription = lens _diaarsDescription (\ s a -> s{_diaarsDescription = a});

-- | FIXME: Undocumented member.
diaarsStatus :: Lens' DescribeImageAttributeResponse Int
diaarsStatus = lens _diaarsStatus (\ s a -> s{_diaarsStatus = a});
