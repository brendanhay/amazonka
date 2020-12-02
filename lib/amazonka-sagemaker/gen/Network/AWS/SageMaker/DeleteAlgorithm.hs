{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified algorithm from your account.
module Network.AWS.SageMaker.DeleteAlgorithm
  ( -- * Creating a Request
    deleteAlgorithm,
    DeleteAlgorithm,

    -- * Request Lenses
    daAlgorithmName,

    -- * Destructuring the Response
    deleteAlgorithmResponse,
    DeleteAlgorithmResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteAlgorithm' smart constructor.
newtype DeleteAlgorithm = DeleteAlgorithm'
  { _daAlgorithmName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAlgorithmName' - The name of the algorithm to delete.
deleteAlgorithm ::
  -- | 'daAlgorithmName'
  Text ->
  DeleteAlgorithm
deleteAlgorithm pAlgorithmName_ =
  DeleteAlgorithm' {_daAlgorithmName = pAlgorithmName_}

-- | The name of the algorithm to delete.
daAlgorithmName :: Lens' DeleteAlgorithm Text
daAlgorithmName = lens _daAlgorithmName (\s a -> s {_daAlgorithmName = a})

instance AWSRequest DeleteAlgorithm where
  type Rs DeleteAlgorithm = DeleteAlgorithmResponse
  request = postJSON sageMaker
  response = receiveNull DeleteAlgorithmResponse'

instance Hashable DeleteAlgorithm

instance NFData DeleteAlgorithm

instance ToHeaders DeleteAlgorithm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteAlgorithm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAlgorithm where
  toJSON DeleteAlgorithm' {..} =
    object (catMaybes [Just ("AlgorithmName" .= _daAlgorithmName)])

instance ToPath DeleteAlgorithm where
  toPath = const "/"

instance ToQuery DeleteAlgorithm where
  toQuery = const mempty

-- | /See:/ 'deleteAlgorithmResponse' smart constructor.
data DeleteAlgorithmResponse = DeleteAlgorithmResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAlgorithmResponse' with the minimum fields required to make a request.
deleteAlgorithmResponse ::
  DeleteAlgorithmResponse
deleteAlgorithmResponse = DeleteAlgorithmResponse'

instance NFData DeleteAlgorithmResponse
