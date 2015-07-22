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
    , diarsLaunchPermissions
    , diarsRAMDiskId
    , diarsKernelId
    , diarsSRIOVNetSupport
    , diarsImageId
    , diarsProductCodes
    , diarsBlockDeviceMappings
    , diarsDescription
    , diarsStatus
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
-- * 'diarsLaunchPermissions'
--
-- * 'diarsRAMDiskId'
--
-- * 'diarsKernelId'
--
-- * 'diarsSRIOVNetSupport'
--
-- * 'diarsImageId'
--
-- * 'diarsProductCodes'
--
-- * 'diarsBlockDeviceMappings'
--
-- * 'diarsDescription'
--
-- * 'diarsStatus'
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
    { _diarsLaunchPermissions   :: !(Maybe [LaunchPermission])
    , _diarsRAMDiskId           :: !(Maybe AttributeValue)
    , _diarsKernelId            :: !(Maybe AttributeValue)
    , _diarsSRIOVNetSupport     :: !(Maybe AttributeValue)
    , _diarsImageId             :: !(Maybe Text)
    , _diarsProductCodes        :: !(Maybe [ProductCode])
    , _diarsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _diarsDescription         :: !(Maybe AttributeValue)
    , _diarsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImageAttributeResponse' smart constructor.
describeImageAttributeResponse :: Int -> DescribeImageAttributeResponse
describeImageAttributeResponse pStatus =
    DescribeImageAttributeResponse'
    { _diarsLaunchPermissions = Nothing
    , _diarsRAMDiskId = Nothing
    , _diarsKernelId = Nothing
    , _diarsSRIOVNetSupport = Nothing
    , _diarsImageId = Nothing
    , _diarsProductCodes = Nothing
    , _diarsBlockDeviceMappings = Nothing
    , _diarsDescription = Nothing
    , _diarsStatus = pStatus
    }

-- | One or more launch permissions.
diarsLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarsLaunchPermissions = lens _diarsLaunchPermissions (\ s a -> s{_diarsLaunchPermissions = a}) . _Default;

-- | The RAM disk ID.
diarsRAMDiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsRAMDiskId = lens _diarsRAMDiskId (\ s a -> s{_diarsRAMDiskId = a});

-- | The kernel ID.
diarsKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsKernelId = lens _diarsKernelId (\ s a -> s{_diarsKernelId = a});

-- | FIXME: Undocumented member.
diarsSRIOVNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsSRIOVNetSupport = lens _diarsSRIOVNetSupport (\ s a -> s{_diarsSRIOVNetSupport = a});

-- | The ID of the AMI.
diarsImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarsImageId = lens _diarsImageId (\ s a -> s{_diarsImageId = a});

-- | One or more product codes.
diarsProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarsProductCodes = lens _diarsProductCodes (\ s a -> s{_diarsProductCodes = a}) . _Default;

-- | One or more block device mapping entries.
diarsBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarsBlockDeviceMappings = lens _diarsBlockDeviceMappings (\ s a -> s{_diarsBlockDeviceMappings = a}) . _Default;

-- | A description for the AMI.
diarsDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsDescription = lens _diarsDescription (\ s a -> s{_diarsDescription = a});

-- | FIXME: Undocumented member.
diarsStatus :: Lens' DescribeImageAttributeResponse Int
diarsStatus = lens _diarsStatus (\ s a -> s{_diarsStatus = a});
