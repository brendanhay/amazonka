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
-- Module      : Network.AWS.ECR.GetRepositoryPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the repository policy for a specified repository.
--
--
module Network.AWS.ECR.GetRepositoryPolicy
    (
    -- * Creating a Request
      getRepositoryPolicy
    , GetRepositoryPolicy
    -- * Request Lenses
    , grpRegistryId
    , grpRepositoryName

    -- * Destructuring the Response
    , getRepositoryPolicyResponse
    , GetRepositoryPolicyResponse
    -- * Response Lenses
    , grprsRegistryId
    , grprsRepositoryName
    , grprsPolicyText
    , grprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { _grpRegistryId     :: !(Maybe Text)
  , _grpRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepositoryPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'grpRepositoryName' - The name of the repository with the policy to retrieve.
getRepositoryPolicy
    :: Text -- ^ 'grpRepositoryName'
    -> GetRepositoryPolicy
getRepositoryPolicy pRepositoryName_ =
  GetRepositoryPolicy'
    {_grpRegistryId = Nothing, _grpRepositoryName = pRepositoryName_}


-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
grpRegistryId :: Lens' GetRepositoryPolicy (Maybe Text)
grpRegistryId = lens _grpRegistryId (\ s a -> s{_grpRegistryId = a})

-- | The name of the repository with the policy to retrieve.
grpRepositoryName :: Lens' GetRepositoryPolicy Text
grpRepositoryName = lens _grpRepositoryName (\ s a -> s{_grpRepositoryName = a})

instance AWSRequest GetRepositoryPolicy where
        type Rs GetRepositoryPolicy =
             GetRepositoryPolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 GetRepositoryPolicyResponse' <$>
                   (x .?> "registryId") <*> (x .?> "repositoryName") <*>
                     (x .?> "policyText")
                     <*> (pure (fromEnum s)))

instance Hashable GetRepositoryPolicy where

instance NFData GetRepositoryPolicy where

instance ToHeaders GetRepositoryPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.GetRepositoryPolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRepositoryPolicy where
        toJSON GetRepositoryPolicy'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _grpRegistryId,
                  Just ("repositoryName" .= _grpRepositoryName)])

instance ToPath GetRepositoryPolicy where
        toPath = const "/"

instance ToQuery GetRepositoryPolicy where
        toQuery = const mempty

-- | /See:/ 'getRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { _grprsRegistryId     :: !(Maybe Text)
  , _grprsRepositoryName :: !(Maybe Text)
  , _grprsPolicyText     :: !(Maybe Text)
  , _grprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsRegistryId' - The registry ID associated with the request.
--
-- * 'grprsRepositoryName' - The repository name associated with the request.
--
-- * 'grprsPolicyText' - The JSON repository policy text associated with the repository.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getRepositoryPolicyResponse
    :: Int -- ^ 'grprsResponseStatus'
    -> GetRepositoryPolicyResponse
getRepositoryPolicyResponse pResponseStatus_ =
  GetRepositoryPolicyResponse'
    { _grprsRegistryId = Nothing
    , _grprsRepositoryName = Nothing
    , _grprsPolicyText = Nothing
    , _grprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
grprsRegistryId :: Lens' GetRepositoryPolicyResponse (Maybe Text)
grprsRegistryId = lens _grprsRegistryId (\ s a -> s{_grprsRegistryId = a})

-- | The repository name associated with the request.
grprsRepositoryName :: Lens' GetRepositoryPolicyResponse (Maybe Text)
grprsRepositoryName = lens _grprsRepositoryName (\ s a -> s{_grprsRepositoryName = a})

-- | The JSON repository policy text associated with the repository.
grprsPolicyText :: Lens' GetRepositoryPolicyResponse (Maybe Text)
grprsPolicyText = lens _grprsPolicyText (\ s a -> s{_grprsPolicyText = a})

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetRepositoryPolicyResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\ s a -> s{_grprsResponseStatus = a})

instance NFData GetRepositoryPolicyResponse where
