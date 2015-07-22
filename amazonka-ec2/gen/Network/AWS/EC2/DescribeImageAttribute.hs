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
    , descDryRun
    , descImageId
    , descAttribute

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
    , diarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImageAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descDryRun'
--
-- * 'descImageId'
--
-- * 'descAttribute'
data DescribeImageAttribute = DescribeImageAttribute'
    { _descDryRun    :: !(Maybe Bool)
    , _descImageId   :: !Text
    , _descAttribute :: !ImageAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImageAttribute' smart constructor.
describeImageAttribute :: Text -> ImageAttributeName -> DescribeImageAttribute
describeImageAttribute pImageId pAttribute =
    DescribeImageAttribute'
    { _descDryRun = Nothing
    , _descImageId = pImageId
    , _descAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
descDryRun :: Lens' DescribeImageAttribute (Maybe Bool)
descDryRun = lens _descDryRun (\ s a -> s{_descDryRun = a});

-- | The ID of the AMI.
descImageId :: Lens' DescribeImageAttribute Text
descImageId = lens _descImageId (\ s a -> s{_descImageId = a});

-- | The AMI attribute.
--
-- __Note__: Depending on your account privileges, the @blockDeviceMapping@
-- attribute may return a @Client.AuthFailure@ error. If this happens, use
-- DescribeImages to get information about the block device mapping for the
-- AMI.
descAttribute :: Lens' DescribeImageAttribute ImageAttributeName
descAttribute = lens _descAttribute (\ s a -> s{_descAttribute = a});

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
               "DryRun" =: _descDryRun, "ImageId" =: _descImageId,
               "Attribute" =: _descAttribute]

-- | Describes an image attribute.
--
-- /See:/ 'describeImageAttributeResponse' smart constructor.
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
--
-- * 'diarStatus'
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
    { _diarLaunchPermissions   :: !(Maybe [LaunchPermission])
    , _diarRAMDiskId           :: !(Maybe AttributeValue)
    , _diarKernelId            :: !(Maybe AttributeValue)
    , _diarSRIOVNetSupport     :: !(Maybe AttributeValue)
    , _diarImageId             :: !(Maybe Text)
    , _diarProductCodes        :: !(Maybe [ProductCode])
    , _diarBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _diarDescription         :: !(Maybe AttributeValue)
    , _diarStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImageAttributeResponse' smart constructor.
describeImageAttributeResponse :: Int -> DescribeImageAttributeResponse
describeImageAttributeResponse pStatus =
    DescribeImageAttributeResponse'
    { _diarLaunchPermissions = Nothing
    , _diarRAMDiskId = Nothing
    , _diarKernelId = Nothing
    , _diarSRIOVNetSupport = Nothing
    , _diarImageId = Nothing
    , _diarProductCodes = Nothing
    , _diarBlockDeviceMappings = Nothing
    , _diarDescription = Nothing
    , _diarStatus = pStatus
    }

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

-- | FIXME: Undocumented member.
diarStatus :: Lens' DescribeImageAttributeResponse Int
diarStatus = lens _diarStatus (\ s a -> s{_diarStatus = a});
