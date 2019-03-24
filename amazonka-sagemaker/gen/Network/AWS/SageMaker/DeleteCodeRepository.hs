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
-- Module      : Network.AWS.SageMaker.DeleteCodeRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Git repository from your account.
--
--
module Network.AWS.SageMaker.DeleteCodeRepository
    (
    -- * Creating a Request
      deleteCodeRepository
    , DeleteCodeRepository
    -- * Request Lenses
    , dCodeRepositoryName

    -- * Destructuring the Response
    , deleteCodeRepositoryResponse
    , DeleteCodeRepositoryResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteCodeRepository' smart constructor.
newtype DeleteCodeRepository = DeleteCodeRepository'
  { _dCodeRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCodeRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCodeRepositoryName' - The name of the Git repository to delete.
deleteCodeRepository
    :: Text -- ^ 'dCodeRepositoryName'
    -> DeleteCodeRepository
deleteCodeRepository pCodeRepositoryName_ =
  DeleteCodeRepository' {_dCodeRepositoryName = pCodeRepositoryName_}


-- | The name of the Git repository to delete.
dCodeRepositoryName :: Lens' DeleteCodeRepository Text
dCodeRepositoryName = lens _dCodeRepositoryName (\ s a -> s{_dCodeRepositoryName = a})

instance AWSRequest DeleteCodeRepository where
        type Rs DeleteCodeRepository =
             DeleteCodeRepositoryResponse
        request = postJSON sageMaker
        response = receiveNull DeleteCodeRepositoryResponse'

instance Hashable DeleteCodeRepository where

instance NFData DeleteCodeRepository where

instance ToHeaders DeleteCodeRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteCodeRepository" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCodeRepository where
        toJSON DeleteCodeRepository'{..}
          = object
              (catMaybes
                 [Just
                    ("CodeRepositoryName" .= _dCodeRepositoryName)])

instance ToPath DeleteCodeRepository where
        toPath = const "/"

instance ToQuery DeleteCodeRepository where
        toQuery = const mempty

-- | /See:/ 'deleteCodeRepositoryResponse' smart constructor.
data DeleteCodeRepositoryResponse =
  DeleteCodeRepositoryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCodeRepositoryResponse' with the minimum fields required to make a request.
--
deleteCodeRepositoryResponse
    :: DeleteCodeRepositoryResponse
deleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'


instance NFData DeleteCodeRepositoryResponse where
