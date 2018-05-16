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
-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is either running or stopped.
--
--
-- If you customized your instance with instance store volumes or EBS volumes in addition to the root device volume, the new AMI contains block device mapping information for those volumes. When you launch an instance from this new AMI, the instance automatically launches with those additional volumes.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating Amazon EBS-Backed Linux AMIs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateImage
    (
    -- * Creating a Request
      createImage
    , CreateImage
    -- * Request Lenses
    , ciiNoReboot
    , ciiDescription
    , ciiBlockDeviceMappings
    , ciiDryRun
    , ciiInstanceId
    , ciiName

    -- * Destructuring the Response
    , createImageResponse
    , CreateImageResponse
    -- * Response Lenses
    , cirsImageId
    , cirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateImage.
--
--
--
-- /See:/ 'createImage' smart constructor.
data CreateImage = CreateImage'
  { _ciiNoReboot            :: !(Maybe Bool)
  , _ciiDescription         :: !(Maybe Text)
  , _ciiBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _ciiDryRun              :: !(Maybe Bool)
  , _ciiInstanceId          :: !Text
  , _ciiName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciiNoReboot' - By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
--
-- * 'ciiDescription' - A description for the new image.
--
-- * 'ciiBlockDeviceMappings' - Information about one or more block device mappings.
--
-- * 'ciiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ciiInstanceId' - The ID of the instance.
--
-- * 'ciiName' - A name for the new image. Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
createImage
    :: Text -- ^ 'ciiInstanceId'
    -> Text -- ^ 'ciiName'
    -> CreateImage
createImage pInstanceId_ pName_ =
  CreateImage'
    { _ciiNoReboot = Nothing
    , _ciiDescription = Nothing
    , _ciiBlockDeviceMappings = Nothing
    , _ciiDryRun = Nothing
    , _ciiInstanceId = pInstanceId_
    , _ciiName = pName_
    }


-- | By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
ciiNoReboot :: Lens' CreateImage (Maybe Bool)
ciiNoReboot = lens _ciiNoReboot (\ s a -> s{_ciiNoReboot = a})

-- | A description for the new image.
ciiDescription :: Lens' CreateImage (Maybe Text)
ciiDescription = lens _ciiDescription (\ s a -> s{_ciiDescription = a})

-- | Information about one or more block device mappings.
ciiBlockDeviceMappings :: Lens' CreateImage [BlockDeviceMapping]
ciiBlockDeviceMappings = lens _ciiBlockDeviceMappings (\ s a -> s{_ciiBlockDeviceMappings = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ciiDryRun :: Lens' CreateImage (Maybe Bool)
ciiDryRun = lens _ciiDryRun (\ s a -> s{_ciiDryRun = a})

-- | The ID of the instance.
ciiInstanceId :: Lens' CreateImage Text
ciiInstanceId = lens _ciiInstanceId (\ s a -> s{_ciiInstanceId = a})

-- | A name for the new image. Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
ciiName :: Lens' CreateImage Text
ciiName = lens _ciiName (\ s a -> s{_ciiName = a})

instance AWSRequest CreateImage where
        type Rs CreateImage = CreateImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance Hashable CreateImage where

instance NFData CreateImage where

instance ToHeaders CreateImage where
        toHeaders = const mempty

instance ToPath CreateImage where
        toPath = const "/"

instance ToQuery CreateImage where
        toQuery CreateImage'{..}
          = mconcat
              ["Action" =: ("CreateImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NoReboot" =: _ciiNoReboot,
               "Description" =: _ciiDescription,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _ciiBlockDeviceMappings),
               "DryRun" =: _ciiDryRun,
               "InstanceId" =: _ciiInstanceId, "Name" =: _ciiName]

-- | Contains the output of CreateImage.
--
--
--
-- /See:/ 'createImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { _cirsImageId        :: !(Maybe Text)
  , _cirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsImageId' - The ID of the new AMI.
--
-- * 'cirsResponseStatus' - -- | The response status code.
createImageResponse
    :: Int -- ^ 'cirsResponseStatus'
    -> CreateImageResponse
createImageResponse pResponseStatus_ =
  CreateImageResponse'
    {_cirsImageId = Nothing, _cirsResponseStatus = pResponseStatus_}


-- | The ID of the new AMI.
cirsImageId :: Lens' CreateImageResponse (Maybe Text)
cirsImageId = lens _cirsImageId (\ s a -> s{_cirsImageId = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateImageResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\ s a -> s{_cirsResponseStatus = a})

instance NFData CreateImageResponse where
