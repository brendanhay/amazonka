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
-- Module      : Network.AWS.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle policy. For information about lifecycle policy syntax, see <http://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle Policy Template> .
--
--
module Network.AWS.ECR.PutLifecyclePolicy
    (
    -- * Creating a Request
      putLifecyclePolicy
    , PutLifecyclePolicy
    -- * Request Lenses
    , plpRegistryId
    , plpRepositoryName
    , plpLifecyclePolicyText

    -- * Destructuring the Response
    , putLifecyclePolicyResponse
    , PutLifecyclePolicyResponse
    -- * Response Lenses
    , plprsRegistryId
    , plprsLifecyclePolicyText
    , plprsRepositoryName
    , plprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { _plpRegistryId          :: !(Maybe Text)
  , _plpRepositoryName      :: !Text
  , _plpLifecyclePolicyText :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
--
-- * 'plpRepositoryName' - The name of the repository to receive the policy.
--
-- * 'plpLifecyclePolicyText' - The JSON repository policy text to apply to the repository.
putLifecyclePolicy
    :: Text -- ^ 'plpRepositoryName'
    -> Text -- ^ 'plpLifecyclePolicyText'
    -> PutLifecyclePolicy
putLifecyclePolicy pRepositoryName_ pLifecyclePolicyText_ =
  PutLifecyclePolicy'
    { _plpRegistryId = Nothing
    , _plpRepositoryName = pRepositoryName_
    , _plpLifecyclePolicyText = pLifecyclePolicyText_
    }


-- | The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
plpRegistryId :: Lens' PutLifecyclePolicy (Maybe Text)
plpRegistryId = lens _plpRegistryId (\ s a -> s{_plpRegistryId = a})

-- | The name of the repository to receive the policy.
plpRepositoryName :: Lens' PutLifecyclePolicy Text
plpRepositoryName = lens _plpRepositoryName (\ s a -> s{_plpRepositoryName = a})

-- | The JSON repository policy text to apply to the repository.
plpLifecyclePolicyText :: Lens' PutLifecyclePolicy Text
plpLifecyclePolicyText = lens _plpLifecyclePolicyText (\ s a -> s{_plpLifecyclePolicyText = a})

instance AWSRequest PutLifecyclePolicy where
        type Rs PutLifecyclePolicy =
             PutLifecyclePolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 PutLifecyclePolicyResponse' <$>
                   (x .?> "registryId") <*>
                     (x .?> "lifecyclePolicyText")
                     <*> (x .?> "repositoryName")
                     <*> (pure (fromEnum s)))

instance Hashable PutLifecyclePolicy where

instance NFData PutLifecyclePolicy where

instance ToHeaders PutLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutLifecyclePolicy where
        toJSON PutLifecyclePolicy'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _plpRegistryId,
                  Just ("repositoryName" .= _plpRepositoryName),
                  Just
                    ("lifecyclePolicyText" .= _plpLifecyclePolicyText)])

instance ToPath PutLifecyclePolicy where
        toPath = const "/"

instance ToQuery PutLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'putLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { _plprsRegistryId          :: !(Maybe Text)
  , _plprsLifecyclePolicyText :: !(Maybe Text)
  , _plprsRepositoryName      :: !(Maybe Text)
  , _plprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plprsRegistryId' - The registry ID associated with the request.
--
-- * 'plprsLifecyclePolicyText' - The JSON repository policy text.
--
-- * 'plprsRepositoryName' - The repository name associated with the request.
--
-- * 'plprsResponseStatus' - -- | The response status code.
putLifecyclePolicyResponse
    :: Int -- ^ 'plprsResponseStatus'
    -> PutLifecyclePolicyResponse
putLifecyclePolicyResponse pResponseStatus_ =
  PutLifecyclePolicyResponse'
    { _plprsRegistryId = Nothing
    , _plprsLifecyclePolicyText = Nothing
    , _plprsRepositoryName = Nothing
    , _plprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
plprsRegistryId :: Lens' PutLifecyclePolicyResponse (Maybe Text)
plprsRegistryId = lens _plprsRegistryId (\ s a -> s{_plprsRegistryId = a})

-- | The JSON repository policy text.
plprsLifecyclePolicyText :: Lens' PutLifecyclePolicyResponse (Maybe Text)
plprsLifecyclePolicyText = lens _plprsLifecyclePolicyText (\ s a -> s{_plprsLifecyclePolicyText = a})

-- | The repository name associated with the request.
plprsRepositoryName :: Lens' PutLifecyclePolicyResponse (Maybe Text)
plprsRepositoryName = lens _plprsRepositoryName (\ s a -> s{_plprsRepositoryName = a})

-- | -- | The response status code.
plprsResponseStatus :: Lens' PutLifecyclePolicyResponse Int
plprsResponseStatus = lens _plprsResponseStatus (\ s a -> s{_plprsResponseStatus = a})

instance NFData PutLifecyclePolicyResponse where
