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
-- Module      : Network.AWS.SageMaker.UpdateCodeRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Git repository with the specified values.
--
--
module Network.AWS.SageMaker.UpdateCodeRepository
    (
    -- * Creating a Request
      updateCodeRepository
    , UpdateCodeRepository
    -- * Request Lenses
    , ucrGitConfig
    , ucrCodeRepositoryName

    -- * Destructuring the Response
    , updateCodeRepositoryResponse
    , UpdateCodeRepositoryResponse
    -- * Response Lenses
    , ucrrsResponseStatus
    , ucrrsCodeRepositoryARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateCodeRepository' smart constructor.
data UpdateCodeRepository = UpdateCodeRepository'
  { _ucrGitConfig          :: !(Maybe GitConfigForUpdate)
  , _ucrCodeRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCodeRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrGitConfig' - The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
--
-- * 'ucrCodeRepositoryName' - The name of the Git repository to update.
updateCodeRepository
    :: Text -- ^ 'ucrCodeRepositoryName'
    -> UpdateCodeRepository
updateCodeRepository pCodeRepositoryName_ =
  UpdateCodeRepository'
    {_ucrGitConfig = Nothing, _ucrCodeRepositoryName = pCodeRepositoryName_}


-- | The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
ucrGitConfig :: Lens' UpdateCodeRepository (Maybe GitConfigForUpdate)
ucrGitConfig = lens _ucrGitConfig (\ s a -> s{_ucrGitConfig = a})

-- | The name of the Git repository to update.
ucrCodeRepositoryName :: Lens' UpdateCodeRepository Text
ucrCodeRepositoryName = lens _ucrCodeRepositoryName (\ s a -> s{_ucrCodeRepositoryName = a})

instance AWSRequest UpdateCodeRepository where
        type Rs UpdateCodeRepository =
             UpdateCodeRepositoryResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 UpdateCodeRepositoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "CodeRepositoryArn"))

instance Hashable UpdateCodeRepository where

instance NFData UpdateCodeRepository where

instance ToHeaders UpdateCodeRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateCodeRepository" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCodeRepository where
        toJSON UpdateCodeRepository'{..}
          = object
              (catMaybes
                 [("GitConfig" .=) <$> _ucrGitConfig,
                  Just
                    ("CodeRepositoryName" .= _ucrCodeRepositoryName)])

instance ToPath UpdateCodeRepository where
        toPath = const "/"

instance ToQuery UpdateCodeRepository where
        toQuery = const mempty

-- | /See:/ 'updateCodeRepositoryResponse' smart constructor.
data UpdateCodeRepositoryResponse = UpdateCodeRepositoryResponse'
  { _ucrrsResponseStatus    :: !Int
  , _ucrrsCodeRepositoryARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrrsResponseStatus' - -- | The response status code.
--
-- * 'ucrrsCodeRepositoryARN' - The ARN of the Git repository.
updateCodeRepositoryResponse
    :: Int -- ^ 'ucrrsResponseStatus'
    -> Text -- ^ 'ucrrsCodeRepositoryARN'
    -> UpdateCodeRepositoryResponse
updateCodeRepositoryResponse pResponseStatus_ pCodeRepositoryARN_ =
  UpdateCodeRepositoryResponse'
    { _ucrrsResponseStatus = pResponseStatus_
    , _ucrrsCodeRepositoryARN = pCodeRepositoryARN_
    }


-- | -- | The response status code.
ucrrsResponseStatus :: Lens' UpdateCodeRepositoryResponse Int
ucrrsResponseStatus = lens _ucrrsResponseStatus (\ s a -> s{_ucrrsResponseStatus = a})

-- | The ARN of the Git repository.
ucrrsCodeRepositoryARN :: Lens' UpdateCodeRepositoryResponse Text
ucrrsCodeRepositoryARN = lens _ucrrsCodeRepositoryARN (\ s a -> s{_ucrrsCodeRepositoryARN = a})

instance NFData UpdateCodeRepositoryResponse where
