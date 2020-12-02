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
-- Module      : Network.AWS.ECR.StartLifecyclePolicyPreview
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a preview of the specified lifecycle policy. This allows you to see the results before creating the lifecycle policy.
--
--
module Network.AWS.ECR.StartLifecyclePolicyPreview
    (
    -- * Creating a Request
      startLifecyclePolicyPreview
    , StartLifecyclePolicyPreview
    -- * Request Lenses
    , slppRegistryId
    , slppLifecyclePolicyText
    , slppRepositoryName

    -- * Destructuring the Response
    , startLifecyclePolicyPreviewResponse
    , StartLifecyclePolicyPreviewResponse
    -- * Response Lenses
    , slpprsStatus
    , slpprsRegistryId
    , slpprsLifecyclePolicyText
    , slpprsRepositoryName
    , slpprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { _slppRegistryId          :: !(Maybe Text)
  , _slppLifecyclePolicyText :: !(Maybe Text)
  , _slppRepositoryName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartLifecyclePolicyPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slppRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'slppLifecyclePolicyText' - The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
--
-- * 'slppRepositoryName' - The name of the repository to be evaluated.
startLifecyclePolicyPreview
    :: Text -- ^ 'slppRepositoryName'
    -> StartLifecyclePolicyPreview
startLifecyclePolicyPreview pRepositoryName_ =
  StartLifecyclePolicyPreview'
    { _slppRegistryId = Nothing
    , _slppLifecyclePolicyText = Nothing
    , _slppRepositoryName = pRepositoryName_
    }


-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
slppRegistryId :: Lens' StartLifecyclePolicyPreview (Maybe Text)
slppRegistryId = lens _slppRegistryId (\ s a -> s{_slppRegistryId = a})

-- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
slppLifecyclePolicyText :: Lens' StartLifecyclePolicyPreview (Maybe Text)
slppLifecyclePolicyText = lens _slppLifecyclePolicyText (\ s a -> s{_slppLifecyclePolicyText = a})

-- | The name of the repository to be evaluated.
slppRepositoryName :: Lens' StartLifecyclePolicyPreview Text
slppRepositoryName = lens _slppRepositoryName (\ s a -> s{_slppRepositoryName = a})

instance AWSRequest StartLifecyclePolicyPreview where
        type Rs StartLifecyclePolicyPreview =
             StartLifecyclePolicyPreviewResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 StartLifecyclePolicyPreviewResponse' <$>
                   (x .?> "status") <*> (x .?> "registryId") <*>
                     (x .?> "lifecyclePolicyText")
                     <*> (x .?> "repositoryName")
                     <*> (pure (fromEnum s)))

instance Hashable StartLifecyclePolicyPreview where

instance NFData StartLifecyclePolicyPreview where

instance ToHeaders StartLifecyclePolicyPreview where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartLifecyclePolicyPreview where
        toJSON StartLifecyclePolicyPreview'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _slppRegistryId,
                  ("lifecyclePolicyText" .=) <$>
                    _slppLifecyclePolicyText,
                  Just ("repositoryName" .= _slppRepositoryName)])

instance ToPath StartLifecyclePolicyPreview where
        toPath = const "/"

instance ToQuery StartLifecyclePolicyPreview where
        toQuery = const mempty

-- | /See:/ 'startLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { _slpprsStatus              :: !(Maybe LifecyclePolicyPreviewStatus)
  , _slpprsRegistryId          :: !(Maybe Text)
  , _slpprsLifecyclePolicyText :: !(Maybe Text)
  , _slpprsRepositoryName      :: !(Maybe Text)
  , _slpprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartLifecyclePolicyPreviewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slpprsStatus' - The status of the lifecycle policy preview request.
--
-- * 'slpprsRegistryId' - The registry ID associated with the request.
--
-- * 'slpprsLifecyclePolicyText' - The JSON repository policy text.
--
-- * 'slpprsRepositoryName' - The repository name associated with the request.
--
-- * 'slpprsResponseStatus' - -- | The response status code.
startLifecyclePolicyPreviewResponse
    :: Int -- ^ 'slpprsResponseStatus'
    -> StartLifecyclePolicyPreviewResponse
startLifecyclePolicyPreviewResponse pResponseStatus_ =
  StartLifecyclePolicyPreviewResponse'
    { _slpprsStatus = Nothing
    , _slpprsRegistryId = Nothing
    , _slpprsLifecyclePolicyText = Nothing
    , _slpprsRepositoryName = Nothing
    , _slpprsResponseStatus = pResponseStatus_
    }


-- | The status of the lifecycle policy preview request.
slpprsStatus :: Lens' StartLifecyclePolicyPreviewResponse (Maybe LifecyclePolicyPreviewStatus)
slpprsStatus = lens _slpprsStatus (\ s a -> s{_slpprsStatus = a})

-- | The registry ID associated with the request.
slpprsRegistryId :: Lens' StartLifecyclePolicyPreviewResponse (Maybe Text)
slpprsRegistryId = lens _slpprsRegistryId (\ s a -> s{_slpprsRegistryId = a})

-- | The JSON repository policy text.
slpprsLifecyclePolicyText :: Lens' StartLifecyclePolicyPreviewResponse (Maybe Text)
slpprsLifecyclePolicyText = lens _slpprsLifecyclePolicyText (\ s a -> s{_slpprsLifecyclePolicyText = a})

-- | The repository name associated with the request.
slpprsRepositoryName :: Lens' StartLifecyclePolicyPreviewResponse (Maybe Text)
slpprsRepositoryName = lens _slpprsRepositoryName (\ s a -> s{_slpprsRepositoryName = a})

-- | -- | The response status code.
slpprsResponseStatus :: Lens' StartLifecyclePolicyPreviewResponse Int
slpprsResponseStatus = lens _slpprsResponseStatus (\ s a -> s{_slpprsResponseStatus = a})

instance NFData StartLifecyclePolicyPreviewResponse
         where
