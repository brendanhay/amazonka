{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified AMI. You can specify only one attribute at a time.
--
--
module Network.AWS.EC2.DescribeImageAttribute
    (
    -- * Creating a Request
      describeImageAttribute
    , DescribeImageAttribute
    -- * Request Lenses
    , diaiDryRun
    , diaiAttribute
    , diaiImageId

    -- * Destructuring the Response
    , describeImageAttributeResponse
    , DescribeImageAttributeResponse
    -- * Response Lenses
    , diarsLaunchPermissions
    , diarsRAMDiskId
    , diarsKernelId
    , diarsSRIOVNetSupport
    , diarsImageId
    , diarsProductCodes
    , diarsDescription
    , diarsBlockDeviceMappings
    , diarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeImageAttribute.
--
--
--
-- /See:/ 'describeImageAttribute' smart constructor.
data DescribeImageAttribute = DescribeImageAttribute'
  { _diaiDryRun    :: !(Maybe Bool)
  , _diaiAttribute :: !ImageAttributeName
  , _diaiImageId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'diaiAttribute' - The AMI attribute. __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
--
-- * 'diaiImageId' - The ID of the AMI.
describeImageAttribute
    :: ImageAttributeName -- ^ 'diaiAttribute'
    -> Text -- ^ 'diaiImageId'
    -> DescribeImageAttribute
describeImageAttribute pAttribute_ pImageId_ =
  DescribeImageAttribute'
    { _diaiDryRun = Nothing
    , _diaiAttribute = pAttribute_
    , _diaiImageId = pImageId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
diaiDryRun :: Lens' DescribeImageAttribute (Maybe Bool)
diaiDryRun = lens _diaiDryRun (\ s a -> s{_diaiDryRun = a})

-- | The AMI attribute. __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
diaiAttribute :: Lens' DescribeImageAttribute ImageAttributeName
diaiAttribute = lens _diaiAttribute (\ s a -> s{_diaiAttribute = a})

-- | The ID of the AMI.
diaiImageId :: Lens' DescribeImageAttribute Text
diaiImageId = lens _diaiImageId (\ s a -> s{_diaiImageId = a})

instance AWSRequest DescribeImageAttribute where
        type Rs DescribeImageAttribute =
             DescribeImageAttributeResponse
        request = postQuery ec2
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
                     <*> (x .@? "description")
                     <*>
                     (x .@? "blockDeviceMapping" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImageAttribute where

instance NFData DescribeImageAttribute where

instance ToHeaders DescribeImageAttribute where
        toHeaders = const mempty

instance ToPath DescribeImageAttribute where
        toPath = const "/"

instance ToQuery DescribeImageAttribute where
        toQuery DescribeImageAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImageAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _diaiDryRun,
               "Attribute" =: _diaiAttribute,
               "ImageId" =: _diaiImageId]

-- | Describes an image attribute.
--
--
--
-- /See:/ 'describeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { _diarsLaunchPermissions   :: !(Maybe [LaunchPermission])
  , _diarsRAMDiskId           :: !(Maybe AttributeValue)
  , _diarsKernelId            :: !(Maybe AttributeValue)
  , _diarsSRIOVNetSupport     :: !(Maybe AttributeValue)
  , _diarsImageId             :: !(Maybe Text)
  , _diarsProductCodes        :: !(Maybe [ProductCode])
  , _diarsDescription         :: !(Maybe AttributeValue)
  , _diarsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _diarsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diarsLaunchPermissions' - One or more launch permissions.
--
-- * 'diarsRAMDiskId' - The RAM disk ID.
--
-- * 'diarsKernelId' - The kernel ID.
--
-- * 'diarsSRIOVNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'diarsImageId' - The ID of the AMI.
--
-- * 'diarsProductCodes' - One or more product codes.
--
-- * 'diarsDescription' - A description for the AMI.
--
-- * 'diarsBlockDeviceMappings' - One or more block device mapping entries.
--
-- * 'diarsResponseStatus' - -- | The response status code.
describeImageAttributeResponse
    :: Int -- ^ 'diarsResponseStatus'
    -> DescribeImageAttributeResponse
describeImageAttributeResponse pResponseStatus_ =
  DescribeImageAttributeResponse'
    { _diarsLaunchPermissions = Nothing
    , _diarsRAMDiskId = Nothing
    , _diarsKernelId = Nothing
    , _diarsSRIOVNetSupport = Nothing
    , _diarsImageId = Nothing
    , _diarsProductCodes = Nothing
    , _diarsDescription = Nothing
    , _diarsBlockDeviceMappings = Nothing
    , _diarsResponseStatus = pResponseStatus_
    }


-- | One or more launch permissions.
diarsLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarsLaunchPermissions = lens _diarsLaunchPermissions (\ s a -> s{_diarsLaunchPermissions = a}) . _Default . _Coerce

-- | The RAM disk ID.
diarsRAMDiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsRAMDiskId = lens _diarsRAMDiskId (\ s a -> s{_diarsRAMDiskId = a})

-- | The kernel ID.
diarsKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsKernelId = lens _diarsKernelId (\ s a -> s{_diarsKernelId = a})

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
diarsSRIOVNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsSRIOVNetSupport = lens _diarsSRIOVNetSupport (\ s a -> s{_diarsSRIOVNetSupport = a})

-- | The ID of the AMI.
diarsImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarsImageId = lens _diarsImageId (\ s a -> s{_diarsImageId = a})

-- | One or more product codes.
diarsProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarsProductCodes = lens _diarsProductCodes (\ s a -> s{_diarsProductCodes = a}) . _Default . _Coerce

-- | A description for the AMI.
diarsDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsDescription = lens _diarsDescription (\ s a -> s{_diarsDescription = a})

-- | One or more block device mapping entries.
diarsBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarsBlockDeviceMappings = lens _diarsBlockDeviceMappings (\ s a -> s{_diarsBlockDeviceMappings = a}) . _Default . _Coerce

-- | -- | The response status code.
diarsResponseStatus :: Lens' DescribeImageAttributeResponse Int
diarsResponseStatus = lens _diarsResponseStatus (\ s a -> s{_diarsResponseStatus = a})

instance NFData DescribeImageAttributeResponse where
