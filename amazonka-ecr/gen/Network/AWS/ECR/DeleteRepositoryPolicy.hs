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
-- Module      : Network.AWS.ECR.DeleteRepositoryPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the repository policy from a specified repository.
--
--
module Network.AWS.ECR.DeleteRepositoryPolicy
    (
    -- * Creating a Request
      deleteRepositoryPolicy
    , DeleteRepositoryPolicy
    -- * Request Lenses
    , drpRegistryId
    , drpRepositoryName

    -- * Destructuring the Response
    , deleteRepositoryPolicyResponse
    , DeleteRepositoryPolicyResponse
    -- * Response Lenses
    , drprsRegistryId
    , drprsRepositoryName
    , drprsPolicyText
    , drprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRepositoryPolicy' smart constructor.
data DeleteRepositoryPolicy = DeleteRepositoryPolicy'
  { _drpRegistryId     :: !(Maybe Text)
  , _drpRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRepositoryPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRegistryId' - The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
--
-- * 'drpRepositoryName' - The name of the repository that is associated with the repository policy to delete.
deleteRepositoryPolicy
    :: Text -- ^ 'drpRepositoryName'
    -> DeleteRepositoryPolicy
deleteRepositoryPolicy pRepositoryName_ =
  DeleteRepositoryPolicy'
    {_drpRegistryId = Nothing, _drpRepositoryName = pRepositoryName_}


-- | The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
drpRegistryId :: Lens' DeleteRepositoryPolicy (Maybe Text)
drpRegistryId = lens _drpRegistryId (\ s a -> s{_drpRegistryId = a})

-- | The name of the repository that is associated with the repository policy to delete.
drpRepositoryName :: Lens' DeleteRepositoryPolicy Text
drpRepositoryName = lens _drpRepositoryName (\ s a -> s{_drpRepositoryName = a})

instance AWSRequest DeleteRepositoryPolicy where
        type Rs DeleteRepositoryPolicy =
             DeleteRepositoryPolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRepositoryPolicyResponse' <$>
                   (x .?> "registryId") <*> (x .?> "repositoryName") <*>
                     (x .?> "policyText")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteRepositoryPolicy where

instance NFData DeleteRepositoryPolicy where

instance ToHeaders DeleteRepositoryPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.DeleteRepositoryPolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRepositoryPolicy where
        toJSON DeleteRepositoryPolicy'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _drpRegistryId,
                  Just ("repositoryName" .= _drpRepositoryName)])

instance ToPath DeleteRepositoryPolicy where
        toPath = const "/"

instance ToQuery DeleteRepositoryPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteRepositoryPolicyResponse' smart constructor.
data DeleteRepositoryPolicyResponse = DeleteRepositoryPolicyResponse'
  { _drprsRegistryId     :: !(Maybe Text)
  , _drprsRepositoryName :: !(Maybe Text)
  , _drprsPolicyText     :: !(Maybe Text)
  , _drprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRepositoryPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsRegistryId' - The registry ID associated with the request.
--
-- * 'drprsRepositoryName' - The repository name associated with the request.
--
-- * 'drprsPolicyText' - The JSON repository policy that was deleted from the repository.
--
-- * 'drprsResponseStatus' - -- | The response status code.
deleteRepositoryPolicyResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DeleteRepositoryPolicyResponse
deleteRepositoryPolicyResponse pResponseStatus_ =
  DeleteRepositoryPolicyResponse'
    { _drprsRegistryId = Nothing
    , _drprsRepositoryName = Nothing
    , _drprsPolicyText = Nothing
    , _drprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
drprsRegistryId :: Lens' DeleteRepositoryPolicyResponse (Maybe Text)
drprsRegistryId = lens _drprsRegistryId (\ s a -> s{_drprsRegistryId = a})

-- | The repository name associated with the request.
drprsRepositoryName :: Lens' DeleteRepositoryPolicyResponse (Maybe Text)
drprsRepositoryName = lens _drprsRepositoryName (\ s a -> s{_drprsRepositoryName = a})

-- | The JSON repository policy that was deleted from the repository.
drprsPolicyText :: Lens' DeleteRepositoryPolicyResponse (Maybe Text)
drprsPolicyText = lens _drprsPolicyText (\ s a -> s{_drprsPolicyText = a})

-- | -- | The response status code.
drprsResponseStatus :: Lens' DeleteRepositoryPolicyResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a})

instance NFData DeleteRepositoryPolicyResponse where
