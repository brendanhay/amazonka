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
-- Module      : Network.AWS.ECR.InitiateLayerUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notify Amazon ECR that you intend to upload an image layer.
--
--
module Network.AWS.ECR.InitiateLayerUpload
    (
    -- * Creating a Request
      initiateLayerUpload
    , InitiateLayerUpload
    -- * Request Lenses
    , iluRegistryId
    , iluRepositoryName

    -- * Destructuring the Response
    , initiateLayerUploadResponse
    , InitiateLayerUploadResponse
    -- * Response Lenses
    , ilursPartSize
    , ilursUploadId
    , ilursResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'initiateLayerUpload' smart constructor.
data InitiateLayerUpload = InitiateLayerUpload'
  { _iluRegistryId     :: !(Maybe Text)
  , _iluRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateLayerUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iluRegistryId' - The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- * 'iluRepositoryName' - The name of the repository to which you intend to upload layers.
initiateLayerUpload
    :: Text -- ^ 'iluRepositoryName'
    -> InitiateLayerUpload
initiateLayerUpload pRepositoryName_ =
  InitiateLayerUpload'
    {_iluRegistryId = Nothing, _iluRepositoryName = pRepositoryName_}


-- | The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
iluRegistryId :: Lens' InitiateLayerUpload (Maybe Text)
iluRegistryId = lens _iluRegistryId (\ s a -> s{_iluRegistryId = a})

-- | The name of the repository to which you intend to upload layers.
iluRepositoryName :: Lens' InitiateLayerUpload Text
iluRepositoryName = lens _iluRepositoryName (\ s a -> s{_iluRepositoryName = a})

instance AWSRequest InitiateLayerUpload where
        type Rs InitiateLayerUpload =
             InitiateLayerUploadResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 InitiateLayerUploadResponse' <$>
                   (x .?> "partSize") <*> (x .?> "uploadId") <*>
                     (pure (fromEnum s)))

instance Hashable InitiateLayerUpload where

instance NFData InitiateLayerUpload where

instance ToHeaders InitiateLayerUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.InitiateLayerUpload"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON InitiateLayerUpload where
        toJSON InitiateLayerUpload'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _iluRegistryId,
                  Just ("repositoryName" .= _iluRepositoryName)])

instance ToPath InitiateLayerUpload where
        toPath = const "/"

instance ToQuery InitiateLayerUpload where
        toQuery = const mempty

-- | /See:/ 'initiateLayerUploadResponse' smart constructor.
data InitiateLayerUploadResponse = InitiateLayerUploadResponse'
  { _ilursPartSize       :: !(Maybe Nat)
  , _ilursUploadId       :: !(Maybe Text)
  , _ilursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateLayerUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilursPartSize' - The size, in bytes, that Amazon ECR expects future layer part uploads to be.
--
-- * 'ilursUploadId' - The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
--
-- * 'ilursResponseStatus' - -- | The response status code.
initiateLayerUploadResponse
    :: Int -- ^ 'ilursResponseStatus'
    -> InitiateLayerUploadResponse
initiateLayerUploadResponse pResponseStatus_ =
  InitiateLayerUploadResponse'
    { _ilursPartSize = Nothing
    , _ilursUploadId = Nothing
    , _ilursResponseStatus = pResponseStatus_
    }


-- | The size, in bytes, that Amazon ECR expects future layer part uploads to be.
ilursPartSize :: Lens' InitiateLayerUploadResponse (Maybe Natural)
ilursPartSize = lens _ilursPartSize (\ s a -> s{_ilursPartSize = a}) . mapping _Nat

-- | The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
ilursUploadId :: Lens' InitiateLayerUploadResponse (Maybe Text)
ilursUploadId = lens _ilursUploadId (\ s a -> s{_ilursUploadId = a})

-- | -- | The response status code.
ilursResponseStatus :: Lens' InitiateLayerUploadResponse Int
ilursResponseStatus = lens _ilursResponseStatus (\ s a -> s{_ilursResponseStatus = a})

instance NFData InitiateLayerUploadResponse where
