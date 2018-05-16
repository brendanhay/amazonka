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
-- Module      : Network.AWS.ECR.CreateRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an image repository.
--
--
module Network.AWS.ECR.CreateRepository
    (
    -- * Creating a Request
      createRepository
    , CreateRepository
    -- * Request Lenses
    , crRepositoryName

    -- * Destructuring the Response
    , createRepositoryResponse
    , CreateRepositoryResponse
    -- * Response Lenses
    , crrsRepository
    , crrsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRepository' smart constructor.
newtype CreateRepository = CreateRepository'
  { _crRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRepositoryName' - The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
createRepository
    :: Text -- ^ 'crRepositoryName'
    -> CreateRepository
createRepository pRepositoryName_ =
  CreateRepository' {_crRepositoryName = pRepositoryName_}


-- | The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
crRepositoryName :: Lens' CreateRepository Text
crRepositoryName = lens _crRepositoryName (\ s a -> s{_crRepositoryName = a})

instance AWSRequest CreateRepository where
        type Rs CreateRepository = CreateRepositoryResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 CreateRepositoryResponse' <$>
                   (x .?> "repository") <*> (pure (fromEnum s)))

instance Hashable CreateRepository where

instance NFData CreateRepository where

instance ToHeaders CreateRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.CreateRepository"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRepository where
        toJSON CreateRepository'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _crRepositoryName)])

instance ToPath CreateRepository where
        toPath = const "/"

instance ToQuery CreateRepository where
        toQuery = const mempty

-- | /See:/ 'createRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { _crrsRepository     :: !(Maybe Repository)
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRepository' - The repository that was created.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRepositoryResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRepositoryResponse
createRepositoryResponse pResponseStatus_ =
  CreateRepositoryResponse'
    {_crrsRepository = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | The repository that was created.
crrsRepository :: Lens' CreateRepositoryResponse (Maybe Repository)
crrsRepository = lens _crrsRepository (\ s a -> s{_crrsRepository = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRepositoryResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateRepositoryResponse where
