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
-- Module      : Network.AWS.ECR.BatchCheckLayerAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Check the availability of multiple image layers in a specified registry and repository.
--
--
module Network.AWS.ECR.BatchCheckLayerAvailability
    (
    -- * Creating a Request
      batchCheckLayerAvailability
    , BatchCheckLayerAvailability
    -- * Request Lenses
    , bclaRegistryId
    , bclaRepositoryName
    , bclaLayerDigests

    -- * Destructuring the Response
    , batchCheckLayerAvailabilityResponse
    , BatchCheckLayerAvailabilityResponse
    -- * Response Lenses
    , bclarsFailures
    , bclarsLayers
    , bclarsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchCheckLayerAvailability' smart constructor.
data BatchCheckLayerAvailability = BatchCheckLayerAvailability'
  { _bclaRegistryId     :: !(Maybe Text)
  , _bclaRepositoryName :: !Text
  , _bclaLayerDigests   :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCheckLayerAvailability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bclaRegistryId' - The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
--
-- * 'bclaRepositoryName' - The name of the repository that is associated with the image layers to check.
--
-- * 'bclaLayerDigests' - The digests of the image layers to check.
batchCheckLayerAvailability
    :: Text -- ^ 'bclaRepositoryName'
    -> NonEmpty Text -- ^ 'bclaLayerDigests'
    -> BatchCheckLayerAvailability
batchCheckLayerAvailability pRepositoryName_ pLayerDigests_ =
  BatchCheckLayerAvailability'
    { _bclaRegistryId = Nothing
    , _bclaRepositoryName = pRepositoryName_
    , _bclaLayerDigests = _List1 # pLayerDigests_
    }


-- | The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
bclaRegistryId :: Lens' BatchCheckLayerAvailability (Maybe Text)
bclaRegistryId = lens _bclaRegistryId (\ s a -> s{_bclaRegistryId = a})

-- | The name of the repository that is associated with the image layers to check.
bclaRepositoryName :: Lens' BatchCheckLayerAvailability Text
bclaRepositoryName = lens _bclaRepositoryName (\ s a -> s{_bclaRepositoryName = a})

-- | The digests of the image layers to check.
bclaLayerDigests :: Lens' BatchCheckLayerAvailability (NonEmpty Text)
bclaLayerDigests = lens _bclaLayerDigests (\ s a -> s{_bclaLayerDigests = a}) . _List1

instance AWSRequest BatchCheckLayerAvailability where
        type Rs BatchCheckLayerAvailability =
             BatchCheckLayerAvailabilityResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 BatchCheckLayerAvailabilityResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "layers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchCheckLayerAvailability where

instance NFData BatchCheckLayerAvailability where

instance ToHeaders BatchCheckLayerAvailability where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.BatchCheckLayerAvailability"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchCheckLayerAvailability where
        toJSON BatchCheckLayerAvailability'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _bclaRegistryId,
                  Just ("repositoryName" .= _bclaRepositoryName),
                  Just ("layerDigests" .= _bclaLayerDigests)])

instance ToPath BatchCheckLayerAvailability where
        toPath = const "/"

instance ToQuery BatchCheckLayerAvailability where
        toQuery = const mempty

-- | /See:/ 'batchCheckLayerAvailabilityResponse' smart constructor.
data BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse'
  { _bclarsFailures       :: !(Maybe [LayerFailure])
  , _bclarsLayers         :: !(Maybe [Layer])
  , _bclarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCheckLayerAvailabilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bclarsFailures' - Any failures associated with the call.
--
-- * 'bclarsLayers' - A list of image layer objects corresponding to the image layer references in the request.
--
-- * 'bclarsResponseStatus' - -- | The response status code.
batchCheckLayerAvailabilityResponse
    :: Int -- ^ 'bclarsResponseStatus'
    -> BatchCheckLayerAvailabilityResponse
batchCheckLayerAvailabilityResponse pResponseStatus_ =
  BatchCheckLayerAvailabilityResponse'
    { _bclarsFailures = Nothing
    , _bclarsLayers = Nothing
    , _bclarsResponseStatus = pResponseStatus_
    }


-- | Any failures associated with the call.
bclarsFailures :: Lens' BatchCheckLayerAvailabilityResponse [LayerFailure]
bclarsFailures = lens _bclarsFailures (\ s a -> s{_bclarsFailures = a}) . _Default . _Coerce

-- | A list of image layer objects corresponding to the image layer references in the request.
bclarsLayers :: Lens' BatchCheckLayerAvailabilityResponse [Layer]
bclarsLayers = lens _bclarsLayers (\ s a -> s{_bclarsLayers = a}) . _Default . _Coerce

-- | -- | The response status code.
bclarsResponseStatus :: Lens' BatchCheckLayerAvailabilityResponse Int
bclarsResponseStatus = lens _bclarsResponseStatus (\ s a -> s{_bclarsResponseStatus = a})

instance NFData BatchCheckLayerAvailabilityResponse
         where
