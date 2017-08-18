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
-- Module      : Network.AWS.EC2.CreateFpgaImage
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FPGA Image (AFI) from the specified design checkpoint (DCP).
--
--
-- The create operation is asynchronous. To verify that the AFI is ready for use, check the output logs.
--
-- An AFI contains the FPGA bitstream that is ready to download to an FPGA. You can securely deploy an AFI on one or more FPGA-accelerated instances. For more information, see the <https://github.com/aws/aws-fpga/ AWS FPGA Hardware Development Kit> .
--
module Network.AWS.EC2.CreateFpgaImage
    (
    -- * Creating a Request
      createFpgaImage
    , CreateFpgaImage
    -- * Request Lenses
    , cfiClientToken
    , cfiLogsStorageLocation
    , cfiName
    , cfiDescription
    , cfiDryRun
    , cfiInputStorageLocation

    -- * Destructuring the Response
    , createFpgaImageResponse
    , CreateFpgaImageResponse
    -- * Response Lenses
    , cfirsFpgaImageId
    , cfirsFpgaImageGlobalId
    , cfirsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
    { _cfiClientToken          :: !(Maybe Text)
    , _cfiLogsStorageLocation  :: !(Maybe StorageLocation)
    , _cfiName                 :: !(Maybe Text)
    , _cfiDescription          :: !(Maybe Text)
    , _cfiDryRun               :: !(Maybe Bool)
    , _cfiInputStorageLocation :: !StorageLocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfiClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cfiLogsStorageLocation' - The location in Amazon S3 for the output logs.
--
-- * 'cfiName' - A name for the AFI.
--
-- * 'cfiDescription' - A description for the AFI.
--
-- * 'cfiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cfiInputStorageLocation' - The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
createFpgaImage
    :: StorageLocation -- ^ 'cfiInputStorageLocation'
    -> CreateFpgaImage
createFpgaImage pInputStorageLocation_ =
    CreateFpgaImage'
    { _cfiClientToken = Nothing
    , _cfiLogsStorageLocation = Nothing
    , _cfiName = Nothing
    , _cfiDescription = Nothing
    , _cfiDryRun = Nothing
    , _cfiInputStorageLocation = pInputStorageLocation_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
cfiClientToken :: Lens' CreateFpgaImage (Maybe Text)
cfiClientToken = lens _cfiClientToken (\ s a -> s{_cfiClientToken = a});

-- | The location in Amazon S3 for the output logs.
cfiLogsStorageLocation :: Lens' CreateFpgaImage (Maybe StorageLocation)
cfiLogsStorageLocation = lens _cfiLogsStorageLocation (\ s a -> s{_cfiLogsStorageLocation = a});

-- | A name for the AFI.
cfiName :: Lens' CreateFpgaImage (Maybe Text)
cfiName = lens _cfiName (\ s a -> s{_cfiName = a});

-- | A description for the AFI.
cfiDescription :: Lens' CreateFpgaImage (Maybe Text)
cfiDescription = lens _cfiDescription (\ s a -> s{_cfiDescription = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cfiDryRun :: Lens' CreateFpgaImage (Maybe Bool)
cfiDryRun = lens _cfiDryRun (\ s a -> s{_cfiDryRun = a});

-- | The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
cfiInputStorageLocation :: Lens' CreateFpgaImage StorageLocation
cfiInputStorageLocation = lens _cfiInputStorageLocation (\ s a -> s{_cfiInputStorageLocation = a});

instance AWSRequest CreateFpgaImage where
        type Rs CreateFpgaImage = CreateFpgaImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateFpgaImageResponse' <$>
                   (x .@? "fpgaImageId") <*> (x .@? "fpgaImageGlobalId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateFpgaImage

instance NFData CreateFpgaImage

instance ToHeaders CreateFpgaImage where
        toHeaders = const mempty

instance ToPath CreateFpgaImage where
        toPath = const "/"

instance ToQuery CreateFpgaImage where
        toQuery CreateFpgaImage'{..}
          = mconcat
              ["Action" =: ("CreateFpgaImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cfiClientToken,
               "LogsStorageLocation" =: _cfiLogsStorageLocation,
               "Name" =: _cfiName, "Description" =: _cfiDescription,
               "DryRun" =: _cfiDryRun,
               "InputStorageLocation" =: _cfiInputStorageLocation]

-- | /See:/ 'createFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
    { _cfirsFpgaImageId       :: !(Maybe Text)
    , _cfirsFpgaImageGlobalId :: !(Maybe Text)
    , _cfirsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFpgaImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfirsFpgaImageId' - The FPGA image identifier (AFI ID).
--
-- * 'cfirsFpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
--
-- * 'cfirsResponseStatus' - -- | The response status code.
createFpgaImageResponse
    :: Int -- ^ 'cfirsResponseStatus'
    -> CreateFpgaImageResponse
createFpgaImageResponse pResponseStatus_ =
    CreateFpgaImageResponse'
    { _cfirsFpgaImageId = Nothing
    , _cfirsFpgaImageGlobalId = Nothing
    , _cfirsResponseStatus = pResponseStatus_
    }

-- | The FPGA image identifier (AFI ID).
cfirsFpgaImageId :: Lens' CreateFpgaImageResponse (Maybe Text)
cfirsFpgaImageId = lens _cfirsFpgaImageId (\ s a -> s{_cfirsFpgaImageId = a});

-- | The global FPGA image identifier (AGFI ID).
cfirsFpgaImageGlobalId :: Lens' CreateFpgaImageResponse (Maybe Text)
cfirsFpgaImageGlobalId = lens _cfirsFpgaImageGlobalId (\ s a -> s{_cfirsFpgaImageGlobalId = a});

-- | -- | The response status code.
cfirsResponseStatus :: Lens' CreateFpgaImageResponse Int
cfirsResponseStatus = lens _cfirsResponseStatus (\ s a -> s{_cfirsResponseStatus = a});

instance NFData CreateFpgaImageResponse
