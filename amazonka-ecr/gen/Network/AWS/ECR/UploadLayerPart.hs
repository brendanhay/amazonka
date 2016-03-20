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
-- Module      : Network.AWS.ECR.UploadLayerPart
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an image layer part to Amazon ECR.
--
-- This operation is used by the Amazon ECR proxy, and it is not intended
-- for general use by customers. Use the 'docker' CLI to pull, tag, and
-- push images.
module Network.AWS.ECR.UploadLayerPart
    (
    -- * Creating a Request
      uploadLayerPart
    , UploadLayerPart
    -- * Request Lenses
    , ulpRegistryId
    , ulpRepositoryName
    , ulpUploadId
    , ulpPartFirstByte
    , ulpPartLastByte
    , ulpLayerPartBlob

    -- * Destructuring the Response
    , uploadLayerPartResponse
    , UploadLayerPartResponse
    -- * Response Lenses
    , ulprsRegistryId
    , ulprsLastByteReceived
    , ulprsRepositoryName
    , ulprsUploadId
    , ulprsResponseStatus
    ) where

import           Network.AWS.ECR.Types
import           Network.AWS.ECR.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadLayerPart' smart constructor.
data UploadLayerPart = UploadLayerPart'
    { _ulpRegistryId     :: !(Maybe Text)
    , _ulpRepositoryName :: !Text
    , _ulpUploadId       :: !Text
    , _ulpPartFirstByte  :: !Nat
    , _ulpPartLastByte   :: !Nat
    , _ulpLayerPartBlob  :: !Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadLayerPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulpRegistryId'
--
-- * 'ulpRepositoryName'
--
-- * 'ulpUploadId'
--
-- * 'ulpPartFirstByte'
--
-- * 'ulpPartLastByte'
--
-- * 'ulpLayerPartBlob'
uploadLayerPart
    :: Text -- ^ 'ulpRepositoryName'
    -> Text -- ^ 'ulpUploadId'
    -> Natural -- ^ 'ulpPartFirstByte'
    -> Natural -- ^ 'ulpPartLastByte'
    -> ByteString -- ^ 'ulpLayerPartBlob'
    -> UploadLayerPart
uploadLayerPart pRepositoryName_ pUploadId_ pPartFirstByte_ pPartLastByte_ pLayerPartBlob_ =
    UploadLayerPart'
    { _ulpRegistryId = Nothing
    , _ulpRepositoryName = pRepositoryName_
    , _ulpUploadId = pUploadId_
    , _ulpPartFirstByte = _Nat # pPartFirstByte_
    , _ulpPartLastByte = _Nat # pPartLastByte_
    , _ulpLayerPartBlob = _Base64 # pLayerPartBlob_
    }

-- | The AWS account ID associated with the registry that you are uploading
-- layer parts to. If you do not specify a registry, the default registry
-- is assumed.
ulpRegistryId :: Lens' UploadLayerPart (Maybe Text)
ulpRegistryId = lens _ulpRegistryId (\ s a -> s{_ulpRegistryId = a});

-- | The name of the repository that you are uploading layer parts to.
ulpRepositoryName :: Lens' UploadLayerPart Text
ulpRepositoryName = lens _ulpRepositoryName (\ s a -> s{_ulpRepositoryName = a});

-- | The upload ID from a previous < InitiateLayerUpload> operation to
-- associate with the layer part upload.
ulpUploadId :: Lens' UploadLayerPart Text
ulpUploadId = lens _ulpUploadId (\ s a -> s{_ulpUploadId = a});

-- | The integer value of the first byte of the layer part.
ulpPartFirstByte :: Lens' UploadLayerPart Natural
ulpPartFirstByte = lens _ulpPartFirstByte (\ s a -> s{_ulpPartFirstByte = a}) . _Nat;

-- | The integer value of the last byte of the layer part.
ulpPartLastByte :: Lens' UploadLayerPart Natural
ulpPartLastByte = lens _ulpPartLastByte (\ s a -> s{_ulpPartLastByte = a}) . _Nat;

-- | The base64-encoded layer part payload.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
ulpLayerPartBlob :: Lens' UploadLayerPart ByteString
ulpLayerPartBlob = lens _ulpLayerPartBlob (\ s a -> s{_ulpLayerPartBlob = a}) . _Base64;

instance AWSRequest UploadLayerPart where
        type Rs UploadLayerPart = UploadLayerPartResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 UploadLayerPartResponse' <$>
                   (x .?> "registryId") <*> (x .?> "lastByteReceived")
                     <*> (x .?> "repositoryName")
                     <*> (x .?> "uploadId")
                     <*> (pure (fromEnum s)))

instance Hashable UploadLayerPart

instance ToHeaders UploadLayerPart where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.UploadLayerPart"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UploadLayerPart where
        toJSON UploadLayerPart'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _ulpRegistryId,
                  Just ("repositoryName" .= _ulpRepositoryName),
                  Just ("uploadId" .= _ulpUploadId),
                  Just ("partFirstByte" .= _ulpPartFirstByte),
                  Just ("partLastByte" .= _ulpPartLastByte),
                  Just ("layerPartBlob" .= _ulpLayerPartBlob)])

instance ToPath UploadLayerPart where
        toPath = const "/"

instance ToQuery UploadLayerPart where
        toQuery = const mempty

-- | /See:/ 'uploadLayerPartResponse' smart constructor.
data UploadLayerPartResponse = UploadLayerPartResponse'
    { _ulprsRegistryId       :: !(Maybe Text)
    , _ulprsLastByteReceived :: !(Maybe Nat)
    , _ulprsRepositoryName   :: !(Maybe Text)
    , _ulprsUploadId         :: !(Maybe Text)
    , _ulprsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadLayerPartResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulprsRegistryId'
--
-- * 'ulprsLastByteReceived'
--
-- * 'ulprsRepositoryName'
--
-- * 'ulprsUploadId'
--
-- * 'ulprsResponseStatus'
uploadLayerPartResponse
    :: Int -- ^ 'ulprsResponseStatus'
    -> UploadLayerPartResponse
uploadLayerPartResponse pResponseStatus_ =
    UploadLayerPartResponse'
    { _ulprsRegistryId = Nothing
    , _ulprsLastByteReceived = Nothing
    , _ulprsRepositoryName = Nothing
    , _ulprsUploadId = Nothing
    , _ulprsResponseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
ulprsRegistryId :: Lens' UploadLayerPartResponse (Maybe Text)
ulprsRegistryId = lens _ulprsRegistryId (\ s a -> s{_ulprsRegistryId = a});

-- | The integer value of the last byte received in the request.
ulprsLastByteReceived :: Lens' UploadLayerPartResponse (Maybe Natural)
ulprsLastByteReceived = lens _ulprsLastByteReceived (\ s a -> s{_ulprsLastByteReceived = a}) . mapping _Nat;

-- | The repository name associated with the request.
ulprsRepositoryName :: Lens' UploadLayerPartResponse (Maybe Text)
ulprsRepositoryName = lens _ulprsRepositoryName (\ s a -> s{_ulprsRepositoryName = a});

-- | The upload ID associated with the request.
ulprsUploadId :: Lens' UploadLayerPartResponse (Maybe Text)
ulprsUploadId = lens _ulprsUploadId (\ s a -> s{_ulprsUploadId = a});

-- | The response status code.
ulprsResponseStatus :: Lens' UploadLayerPartResponse Int
ulprsResponseStatus = lens _ulprsResponseStatus (\ s a -> s{_ulprsResponseStatus = a});
