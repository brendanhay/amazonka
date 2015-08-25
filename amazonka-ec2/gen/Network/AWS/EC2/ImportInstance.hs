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
-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import instance task using metadata from the specified disk
-- image. 'ImportInstance' only supports single-volume VMs. To import
-- multi-volume VMs, use ImportImage. After importing the image, you then
-- upload it using the 'ec2-import-volume' command in the EC2 command line
-- tools. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportInstance.html AWS API Reference> for ImportInstance.
module Network.AWS.EC2.ImportInstance
    (
    -- * Creating a Request
      importInstance
    , ImportInstance
    -- * Request Lenses
    , iiLaunchSpecification
    , iiDiskImages
    , iiDryRun
    , iiDescription
    , iiPlatform

    -- * Destructuring the Response
    , importInstanceResponse
    , ImportInstanceResponse
    -- * Response Lenses
    , iirsConversionTask
    , iirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importInstance' smart constructor.
data ImportInstance = ImportInstance'
    { _iiLaunchSpecification :: !(Maybe ImportInstanceLaunchSpecification)
    , _iiDiskImages          :: !(Maybe [DiskImage])
    , _iiDryRun              :: !(Maybe Bool)
    , _iiDescription         :: !(Maybe Text)
    , _iiPlatform            :: !PlatformValues
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiLaunchSpecification'
--
-- * 'iiDiskImages'
--
-- * 'iiDryRun'
--
-- * 'iiDescription'
--
-- * 'iiPlatform'
importInstance
    :: PlatformValues -- ^ 'iiPlatform'
    -> ImportInstance
importInstance pPlatform_ =
    ImportInstance'
    { _iiLaunchSpecification = Nothing
    , _iiDiskImages = Nothing
    , _iiDryRun = Nothing
    , _iiDescription = Nothing
    , _iiPlatform = pPlatform_
    }

-- | The launch specification.
iiLaunchSpecification :: Lens' ImportInstance (Maybe ImportInstanceLaunchSpecification)
iiLaunchSpecification = lens _iiLaunchSpecification (\ s a -> s{_iiLaunchSpecification = a});

-- | The disk image.
iiDiskImages :: Lens' ImportInstance [DiskImage]
iiDiskImages = lens _iiDiskImages (\ s a -> s{_iiDiskImages = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
iiDryRun :: Lens' ImportInstance (Maybe Bool)
iiDryRun = lens _iiDryRun (\ s a -> s{_iiDryRun = a});

-- | A description for the instance being imported.
iiDescription :: Lens' ImportInstance (Maybe Text)
iiDescription = lens _iiDescription (\ s a -> s{_iiDescription = a});

-- | The instance operating system.
iiPlatform :: Lens' ImportInstance PlatformValues
iiPlatform = lens _iiPlatform (\ s a -> s{_iiPlatform = a});

instance AWSRequest ImportInstance where
        type Rs ImportInstance = ImportInstanceResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 ImportInstanceResponse' <$>
                   (x .@? "conversionTask") <*> (pure (fromEnum s)))

instance ToHeaders ImportInstance where
        toHeaders = const mempty

instance ToPath ImportInstance where
        toPath = const "/"

instance ToQuery ImportInstance where
        toQuery ImportInstance'{..}
          = mconcat
              ["Action" =: ("ImportInstance" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "LaunchSpecification" =: _iiLaunchSpecification,
               toQuery (toQueryList "DiskImage" <$> _iiDiskImages),
               "DryRun" =: _iiDryRun,
               "Description" =: _iiDescription,
               "Platform" =: _iiPlatform]

-- | /See:/ 'importInstanceResponse' smart constructor.
data ImportInstanceResponse = ImportInstanceResponse'
    { _iirsConversionTask :: !(Maybe ConversionTask)
    , _iirsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iirsConversionTask'
--
-- * 'iirsStatus'
importInstanceResponse
    :: Int -- ^ 'iirsStatus'
    -> ImportInstanceResponse
importInstanceResponse pStatus_ =
    ImportInstanceResponse'
    { _iirsConversionTask = Nothing
    , _iirsStatus = pStatus_
    }

-- | Information about the conversion task.
iirsConversionTask :: Lens' ImportInstanceResponse (Maybe ConversionTask)
iirsConversionTask = lens _iirsConversionTask (\ s a -> s{_iirsConversionTask = a});

-- | The response status code.
iirsStatus :: Lens' ImportInstanceResponse Int
iirsStatus = lens _iirsStatus (\ s a -> s{_iirsStatus = a});
