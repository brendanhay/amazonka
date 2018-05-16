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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , creClientToken
    , creLogsStorageLocation
    , creName
    , creDescription
    , creDryRun
    , creInputStorageLocation

    -- * Destructuring the Response
    , createFpgaImageResponse
    , CreateFpgaImageResponse
    -- * Response Lenses
    , cfirsFpgaImageId
    , cfirsFpgaImageGlobalId
    , cfirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
  { _creClientToken          :: !(Maybe Text)
  , _creLogsStorageLocation  :: !(Maybe StorageLocation)
  , _creName                 :: !(Maybe Text)
  , _creDescription          :: !(Maybe Text)
  , _creDryRun               :: !(Maybe Bool)
  , _creInputStorageLocation :: !StorageLocation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'creLogsStorageLocation' - The location in Amazon S3 for the output logs.
--
-- * 'creName' - A name for the AFI.
--
-- * 'creDescription' - A description for the AFI.
--
-- * 'creDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'creInputStorageLocation' - The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
createFpgaImage
    :: StorageLocation -- ^ 'creInputStorageLocation'
    -> CreateFpgaImage
createFpgaImage pInputStorageLocation_ =
  CreateFpgaImage'
    { _creClientToken = Nothing
    , _creLogsStorageLocation = Nothing
    , _creName = Nothing
    , _creDescription = Nothing
    , _creDryRun = Nothing
    , _creInputStorageLocation = pInputStorageLocation_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
creClientToken :: Lens' CreateFpgaImage (Maybe Text)
creClientToken = lens _creClientToken (\ s a -> s{_creClientToken = a})

-- | The location in Amazon S3 for the output logs.
creLogsStorageLocation :: Lens' CreateFpgaImage (Maybe StorageLocation)
creLogsStorageLocation = lens _creLogsStorageLocation (\ s a -> s{_creLogsStorageLocation = a})

-- | A name for the AFI.
creName :: Lens' CreateFpgaImage (Maybe Text)
creName = lens _creName (\ s a -> s{_creName = a})

-- | A description for the AFI.
creDescription :: Lens' CreateFpgaImage (Maybe Text)
creDescription = lens _creDescription (\ s a -> s{_creDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
creDryRun :: Lens' CreateFpgaImage (Maybe Bool)
creDryRun = lens _creDryRun (\ s a -> s{_creDryRun = a})

-- | The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
creInputStorageLocation :: Lens' CreateFpgaImage StorageLocation
creInputStorageLocation = lens _creInputStorageLocation (\ s a -> s{_creInputStorageLocation = a})

instance AWSRequest CreateFpgaImage where
        type Rs CreateFpgaImage = CreateFpgaImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateFpgaImageResponse' <$>
                   (x .@? "fpgaImageId") <*> (x .@? "fpgaImageGlobalId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateFpgaImage where

instance NFData CreateFpgaImage where

instance ToHeaders CreateFpgaImage where
        toHeaders = const mempty

instance ToPath CreateFpgaImage where
        toPath = const "/"

instance ToQuery CreateFpgaImage where
        toQuery CreateFpgaImage'{..}
          = mconcat
              ["Action" =: ("CreateFpgaImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _creClientToken,
               "LogsStorageLocation" =: _creLogsStorageLocation,
               "Name" =: _creName, "Description" =: _creDescription,
               "DryRun" =: _creDryRun,
               "InputStorageLocation" =: _creInputStorageLocation]

-- | /See:/ 'createFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
  { _cfirsFpgaImageId       :: !(Maybe Text)
  , _cfirsFpgaImageGlobalId :: !(Maybe Text)
  , _cfirsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
cfirsFpgaImageId = lens _cfirsFpgaImageId (\ s a -> s{_cfirsFpgaImageId = a})

-- | The global FPGA image identifier (AGFI ID).
cfirsFpgaImageGlobalId :: Lens' CreateFpgaImageResponse (Maybe Text)
cfirsFpgaImageGlobalId = lens _cfirsFpgaImageGlobalId (\ s a -> s{_cfirsFpgaImageGlobalId = a})

-- | -- | The response status code.
cfirsResponseStatus :: Lens' CreateFpgaImageResponse Int
cfirsResponseStatus = lens _cfirsResponseStatus (\ s a -> s{_cfirsResponseStatus = a})

instance NFData CreateFpgaImageResponse where
