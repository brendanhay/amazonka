{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that
-- is either running or stopped.
--
-- If you customized your instance with instance store volumes or EBS
-- volumes in addition to the root device volume, the new AMI contains
-- block device mapping information for those volumes. When you launch an
-- instance from this new AMI, the instance automatically launches with
-- those additional volumes.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating Amazon EBS-Backed Linux AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateImage.html>
module Network.AWS.EC2.CreateImage
    (
    -- * Request
      CreateImage
    -- ** Request constructor
    , createImage
    -- ** Request lenses
    , ciirqNoReboot
    , ciirqBlockDeviceMappings
    , ciirqDryRun
    , ciirqDescription
    , ciirqInstanceId
    , ciirqName

    -- * Response
    , CreateImageResponse
    -- ** Response constructor
    , createImageResponse
    -- ** Response lenses
    , cirsImageId
    , cirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciirqNoReboot'
--
-- * 'ciirqBlockDeviceMappings'
--
-- * 'ciirqDryRun'
--
-- * 'ciirqDescription'
--
-- * 'ciirqInstanceId'
--
-- * 'ciirqName'
data CreateImage = CreateImage'
    { _ciirqNoReboot            :: !(Maybe Bool)
    , _ciirqBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _ciirqDryRun              :: !(Maybe Bool)
    , _ciirqDescription         :: !(Maybe Text)
    , _ciirqInstanceId          :: !Text
    , _ciirqName                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateImage' smart constructor.
createImage :: Text -> Text -> CreateImage
createImage pInstanceId pName =
    CreateImage'
    { _ciirqNoReboot = Nothing
    , _ciirqBlockDeviceMappings = Nothing
    , _ciirqDryRun = Nothing
    , _ciirqDescription = Nothing
    , _ciirqInstanceId = pInstanceId
    , _ciirqName = pName
    }

-- | By default, this parameter is set to @false@, which means Amazon EC2
-- attempts to shut down the instance cleanly before image creation and
-- then reboots the instance. When the parameter is set to @true@, Amazon
-- EC2 doesn\'t shut down the instance before creating the image. When this
-- option is used, file system integrity on the created image can\'t be
-- guaranteed.
ciirqNoReboot :: Lens' CreateImage (Maybe Bool)
ciirqNoReboot = lens _ciirqNoReboot (\ s a -> s{_ciirqNoReboot = a});

-- | Information about one or more block device mappings.
ciirqBlockDeviceMappings :: Lens' CreateImage [BlockDeviceMapping]
ciirqBlockDeviceMappings = lens _ciirqBlockDeviceMappings (\ s a -> s{_ciirqBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ciirqDryRun :: Lens' CreateImage (Maybe Bool)
ciirqDryRun = lens _ciirqDryRun (\ s a -> s{_ciirqDryRun = a});

-- | A description for the new image.
ciirqDescription :: Lens' CreateImage (Maybe Text)
ciirqDescription = lens _ciirqDescription (\ s a -> s{_ciirqDescription = a});

-- | The ID of the instance.
ciirqInstanceId :: Lens' CreateImage Text
ciirqInstanceId = lens _ciirqInstanceId (\ s a -> s{_ciirqInstanceId = a});

-- | A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
ciirqName :: Lens' CreateImage Text
ciirqName = lens _ciirqName (\ s a -> s{_ciirqName = a});

instance AWSRequest CreateImage where
        type Sv CreateImage = EC2
        type Rs CreateImage = CreateImageResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance ToHeaders CreateImage where
        toHeaders = const mempty

instance ToPath CreateImage where
        toPath = const "/"

instance ToQuery CreateImage where
        toQuery CreateImage'{..}
          = mconcat
              ["Action" =: ("CreateImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NoReboot" =: _ciirqNoReboot,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _ciirqBlockDeviceMappings),
               "DryRun" =: _ciirqDryRun,
               "Description" =: _ciirqDescription,
               "InstanceId" =: _ciirqInstanceId,
               "Name" =: _ciirqName]

-- | /See:/ 'createImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirsImageId'
--
-- * 'cirsStatus'
data CreateImageResponse = CreateImageResponse'
    { _cirsImageId :: !(Maybe Text)
    , _cirsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateImageResponse' smart constructor.
createImageResponse :: Int -> CreateImageResponse
createImageResponse pStatus =
    CreateImageResponse'
    { _cirsImageId = Nothing
    , _cirsStatus = pStatus
    }

-- | The ID of the new AMI.
cirsImageId :: Lens' CreateImageResponse (Maybe Text)
cirsImageId = lens _cirsImageId (\ s a -> s{_cirsImageId = a});

-- | FIXME: Undocumented member.
cirsStatus :: Lens' CreateImageResponse Int
cirsStatus = lens _cirsStatus (\ s a -> s{_cirsStatus = a});
