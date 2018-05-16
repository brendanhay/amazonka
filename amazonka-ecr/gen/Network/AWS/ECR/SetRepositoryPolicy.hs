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
-- Module      : Network.AWS.ECR.SetRepositoryPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a repository policy on a specified repository to control access permissions.
--
--
module Network.AWS.ECR.SetRepositoryPolicy
    (
    -- * Creating a Request
      setRepositoryPolicy
    , SetRepositoryPolicy
    -- * Request Lenses
    , srpForce
    , srpRegistryId
    , srpRepositoryName
    , srpPolicyText

    -- * Destructuring the Response
    , setRepositoryPolicyResponse
    , SetRepositoryPolicyResponse
    -- * Response Lenses
    , srprsRegistryId
    , srprsRepositoryName
    , srprsPolicyText
    , srprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { _srpForce          :: !(Maybe Bool)
  , _srpRegistryId     :: !(Maybe Text)
  , _srpRepositoryName :: !Text
  , _srpPolicyText     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRepositoryPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srpForce' - If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
--
-- * 'srpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'srpRepositoryName' - The name of the repository to receive the policy.
--
-- * 'srpPolicyText' - The JSON repository policy text to apply to the repository.
setRepositoryPolicy
    :: Text -- ^ 'srpRepositoryName'
    -> Text -- ^ 'srpPolicyText'
    -> SetRepositoryPolicy
setRepositoryPolicy pRepositoryName_ pPolicyText_ =
  SetRepositoryPolicy'
    { _srpForce = Nothing
    , _srpRegistryId = Nothing
    , _srpRepositoryName = pRepositoryName_
    , _srpPolicyText = pPolicyText_
    }


-- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
srpForce :: Lens' SetRepositoryPolicy (Maybe Bool)
srpForce = lens _srpForce (\ s a -> s{_srpForce = a})

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
srpRegistryId :: Lens' SetRepositoryPolicy (Maybe Text)
srpRegistryId = lens _srpRegistryId (\ s a -> s{_srpRegistryId = a})

-- | The name of the repository to receive the policy.
srpRepositoryName :: Lens' SetRepositoryPolicy Text
srpRepositoryName = lens _srpRepositoryName (\ s a -> s{_srpRepositoryName = a})

-- | The JSON repository policy text to apply to the repository.
srpPolicyText :: Lens' SetRepositoryPolicy Text
srpPolicyText = lens _srpPolicyText (\ s a -> s{_srpPolicyText = a})

instance AWSRequest SetRepositoryPolicy where
        type Rs SetRepositoryPolicy =
             SetRepositoryPolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 SetRepositoryPolicyResponse' <$>
                   (x .?> "registryId") <*> (x .?> "repositoryName") <*>
                     (x .?> "policyText")
                     <*> (pure (fromEnum s)))

instance Hashable SetRepositoryPolicy where

instance NFData SetRepositoryPolicy where

instance ToHeaders SetRepositoryPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetRepositoryPolicy where
        toJSON SetRepositoryPolicy'{..}
          = object
              (catMaybes
                 [("force" .=) <$> _srpForce,
                  ("registryId" .=) <$> _srpRegistryId,
                  Just ("repositoryName" .= _srpRepositoryName),
                  Just ("policyText" .= _srpPolicyText)])

instance ToPath SetRepositoryPolicy where
        toPath = const "/"

instance ToQuery SetRepositoryPolicy where
        toQuery = const mempty

-- | /See:/ 'setRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { _srprsRegistryId     :: !(Maybe Text)
  , _srprsRepositoryName :: !(Maybe Text)
  , _srprsPolicyText     :: !(Maybe Text)
  , _srprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srprsRegistryId' - The registry ID associated with the request.
--
-- * 'srprsRepositoryName' - The repository name associated with the request.
--
-- * 'srprsPolicyText' - The JSON repository policy text applied to the repository.
--
-- * 'srprsResponseStatus' - -- | The response status code.
setRepositoryPolicyResponse
    :: Int -- ^ 'srprsResponseStatus'
    -> SetRepositoryPolicyResponse
setRepositoryPolicyResponse pResponseStatus_ =
  SetRepositoryPolicyResponse'
    { _srprsRegistryId = Nothing
    , _srprsRepositoryName = Nothing
    , _srprsPolicyText = Nothing
    , _srprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
srprsRegistryId :: Lens' SetRepositoryPolicyResponse (Maybe Text)
srprsRegistryId = lens _srprsRegistryId (\ s a -> s{_srprsRegistryId = a})

-- | The repository name associated with the request.
srprsRepositoryName :: Lens' SetRepositoryPolicyResponse (Maybe Text)
srprsRepositoryName = lens _srprsRepositoryName (\ s a -> s{_srprsRepositoryName = a})

-- | The JSON repository policy text applied to the repository.
srprsPolicyText :: Lens' SetRepositoryPolicyResponse (Maybe Text)
srprsPolicyText = lens _srprsPolicyText (\ s a -> s{_srprsPolicyText = a})

-- | -- | The response status code.
srprsResponseStatus :: Lens' SetRepositoryPolicyResponse Int
srprsResponseStatus = lens _srprsResponseStatus (\ s a -> s{_srprsResponseStatus = a})

instance NFData SetRepositoryPolicyResponse where
