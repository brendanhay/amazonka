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
-- Module      : Network.AWS.ECR.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle policy.
--
--
module Network.AWS.ECR.DeleteLifecyclePolicy
    (
    -- * Creating a Request
      deleteLifecyclePolicy
    , DeleteLifecyclePolicy
    -- * Request Lenses
    , dlpRegistryId
    , dlpRepositoryName

    -- * Destructuring the Response
    , deleteLifecyclePolicyResponse
    , DeleteLifecyclePolicyResponse
    -- * Response Lenses
    , dlprsRegistryId
    , dlprsLastEvaluatedAt
    , dlprsLifecyclePolicyText
    , dlprsRepositoryName
    , dlprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { _dlpRegistryId     :: !(Maybe Text)
  , _dlpRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'dlpRepositoryName' - The name of the repository.
deleteLifecyclePolicy
    :: Text -- ^ 'dlpRepositoryName'
    -> DeleteLifecyclePolicy
deleteLifecyclePolicy pRepositoryName_ =
  DeleteLifecyclePolicy'
    {_dlpRegistryId = Nothing, _dlpRepositoryName = pRepositoryName_}


-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
dlpRegistryId :: Lens' DeleteLifecyclePolicy (Maybe Text)
dlpRegistryId = lens _dlpRegistryId (\ s a -> s{_dlpRegistryId = a})

-- | The name of the repository.
dlpRepositoryName :: Lens' DeleteLifecyclePolicy Text
dlpRepositoryName = lens _dlpRepositoryName (\ s a -> s{_dlpRepositoryName = a})

instance AWSRequest DeleteLifecyclePolicy where
        type Rs DeleteLifecyclePolicy =
             DeleteLifecyclePolicyResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 DeleteLifecyclePolicyResponse' <$>
                   (x .?> "registryId") <*> (x .?> "lastEvaluatedAt")
                     <*> (x .?> "lifecyclePolicyText")
                     <*> (x .?> "repositoryName")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteLifecyclePolicy where

instance NFData DeleteLifecyclePolicy where

instance ToHeaders DeleteLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLifecyclePolicy where
        toJSON DeleteLifecyclePolicy'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _dlpRegistryId,
                  Just ("repositoryName" .= _dlpRepositoryName)])

instance ToPath DeleteLifecyclePolicy where
        toPath = const "/"

instance ToQuery DeleteLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'deleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { _dlprsRegistryId          :: !(Maybe Text)
  , _dlprsLastEvaluatedAt     :: !(Maybe POSIX)
  , _dlprsLifecyclePolicyText :: !(Maybe Text)
  , _dlprsRepositoryName      :: !(Maybe Text)
  , _dlprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlprsRegistryId' - The registry ID associated with the request.
--
-- * 'dlprsLastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- * 'dlprsLifecyclePolicyText' - The JSON lifecycle policy text.
--
-- * 'dlprsRepositoryName' - The repository name associated with the request.
--
-- * 'dlprsResponseStatus' - -- | The response status code.
deleteLifecyclePolicyResponse
    :: Int -- ^ 'dlprsResponseStatus'
    -> DeleteLifecyclePolicyResponse
deleteLifecyclePolicyResponse pResponseStatus_ =
  DeleteLifecyclePolicyResponse'
    { _dlprsRegistryId = Nothing
    , _dlprsLastEvaluatedAt = Nothing
    , _dlprsLifecyclePolicyText = Nothing
    , _dlprsRepositoryName = Nothing
    , _dlprsResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
dlprsRegistryId :: Lens' DeleteLifecyclePolicyResponse (Maybe Text)
dlprsRegistryId = lens _dlprsRegistryId (\ s a -> s{_dlprsRegistryId = a})

-- | The time stamp of the last time that the lifecycle policy was run.
dlprsLastEvaluatedAt :: Lens' DeleteLifecyclePolicyResponse (Maybe UTCTime)
dlprsLastEvaluatedAt = lens _dlprsLastEvaluatedAt (\ s a -> s{_dlprsLastEvaluatedAt = a}) . mapping _Time

-- | The JSON lifecycle policy text.
dlprsLifecyclePolicyText :: Lens' DeleteLifecyclePolicyResponse (Maybe Text)
dlprsLifecyclePolicyText = lens _dlprsLifecyclePolicyText (\ s a -> s{_dlprsLifecyclePolicyText = a})

-- | The repository name associated with the request.
dlprsRepositoryName :: Lens' DeleteLifecyclePolicyResponse (Maybe Text)
dlprsRepositoryName = lens _dlprsRepositoryName (\ s a -> s{_dlprsRepositoryName = a})

-- | -- | The response status code.
dlprsResponseStatus :: Lens' DeleteLifecyclePolicyResponse Int
dlprsResponseStatus = lens _dlprsResponseStatus (\ s a -> s{_dlprsResponseStatus = a})

instance NFData DeleteLifecyclePolicyResponse where
