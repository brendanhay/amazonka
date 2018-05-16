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
-- Module      : Network.AWS.ECR.GetLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified lifecycle policy.
--
--
module Network.AWS.ECR.GetLifecyclePolicy
    (
    -- * Creating a Request
      getLifecyclePolicy
    , GetLifecyclePolicy
    -- * Request Lenses
    , glpRegistryId
    , glpRepositoryName

    -- * Destructuring the Response
    , getLifecyclePolicyResponse
    , GetLifecyclePolicyResponse
    -- * Response Lenses
    , glprsRegistryId
    , glprsLastEvaluatedAt
    , glprsLifecyclePolicyText
    , glprsRepositoryName
    , glprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { _glpRegistryId     :: !(Maybe Text)
  , _glpRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'glpRepositoryName' - The name of the repository.
getLifecyclePolicy
    :: Text -- ^ 'glpRepositoryName'
    -> GetLifecyclePolicy
getLifecyclePolicy pRepositoryName_ =
  GetLifecyclePolicy'
    {_glpRegistryId = Nothing, _glpRepositoryName = pRepositoryName_}


-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
glpRegistryId :: Lens' GetLifecyclePolicy (Maybe Text)
glpRegistryId = lens _glpRegistryId (\ s a -> s{_glpRegistryId = a})

-- | The name of the repository.
glpRepositoryName :: Lens' GetLifecyclePolicy Text
glpRepositoryName = lens _glpRepositoryName (\ s a -> s{_glpRepositoryName = a})

instance AWSRequest GetLifecyclePolicy where
        type Rs GetLifecyclePolicy =
             GetLifecyclePolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyResponse' <$>
                   (x .?> "registryId") <*> (x .?> "lastEvaluatedAt")
                     <*> (x .?> "lifecyclePolicyText")
                     <*> (x .?> "repositoryName")
                     <*> (pure (fromEnum s)))

instance Hashable GetLifecyclePolicy where

instance NFData GetLifecyclePolicy where

instance ToHeaders GetLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLifecyclePolicy where
        toJSON GetLifecyclePolicy'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _glpRegistryId,
                  Just ("repositoryName" .= _glpRepositoryName)])

instance ToPath GetLifecyclePolicy where
        toPath = const "/"

instance ToQuery GetLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'getLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { _glprsRegistryId          :: !(Maybe Text)
  , _glprsLastEvaluatedAt     :: !(Maybe POSIX)
  , _glprsLifecyclePolicyText :: !(Maybe Text)
  , _glprsRepositoryName      :: !(Maybe Text)
  , _glprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glprsRegistryId' - The registry ID associated with the request.
--
-- * 'glprsLastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- * 'glprsLifecyclePolicyText' - The JSON lifecycle policy text.
--
-- * 'glprsRepositoryName' - The repository name associated with the request.
--
-- * 'glprsResponseStatus' - -- | The response status code.
getLifecyclePolicyResponse
    :: Int -- ^ 'glprsResponseStatus'
    -> GetLifecyclePolicyResponse
getLifecyclePolicyResponse pResponseStatus_ =
  GetLifecyclePolicyResponse'
    { _glprsRegistryId = Nothing
    , _glprsLastEvaluatedAt = Nothing
    , _glprsLifecyclePolicyText = Nothing
    , _glprsRepositoryName = Nothing
    , _glprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
glprsRegistryId :: Lens' GetLifecyclePolicyResponse (Maybe Text)
glprsRegistryId = lens _glprsRegistryId (\ s a -> s{_glprsRegistryId = a})

-- | The time stamp of the last time that the lifecycle policy was run.
glprsLastEvaluatedAt :: Lens' GetLifecyclePolicyResponse (Maybe UTCTime)
glprsLastEvaluatedAt = lens _glprsLastEvaluatedAt (\ s a -> s{_glprsLastEvaluatedAt = a}) . mapping _Time

-- | The JSON lifecycle policy text.
glprsLifecyclePolicyText :: Lens' GetLifecyclePolicyResponse (Maybe Text)
glprsLifecyclePolicyText = lens _glprsLifecyclePolicyText (\ s a -> s{_glprsLifecyclePolicyText = a})

-- | The repository name associated with the request.
glprsRepositoryName :: Lens' GetLifecyclePolicyResponse (Maybe Text)
glprsRepositoryName = lens _glprsRepositoryName (\ s a -> s{_glprsRepositoryName = a})

-- | -- | The response status code.
glprsResponseStatus :: Lens' GetLifecyclePolicyResponse Int
glprsResponseStatus = lens _glprsResponseStatus (\ s a -> s{_glprsResponseStatus = a})

instance NFData GetLifecyclePolicyResponse where
