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
-- Module      : Network.AWS.Connect.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Connect instance.
module Network.AWS.Connect.DeleteInstance
  ( -- * Creating a Request
    deleteInstance,
    DeleteInstance,

    -- * Request Lenses
    dInstanceId,

    -- * Destructuring the Response
    deleteInstanceResponse,
    DeleteInstanceResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstance' smart constructor.
newtype DeleteInstance = DeleteInstance' {_dInstanceId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInstanceId' - The identifier of the Amazon Connect instance.
deleteInstance ::
  -- | 'dInstanceId'
  Text ->
  DeleteInstance
deleteInstance pInstanceId_ =
  DeleteInstance' {_dInstanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
dInstanceId :: Lens' DeleteInstance Text
dInstanceId = lens _dInstanceId (\s a -> s {_dInstanceId = a})

instance AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = delete connect
  response = receiveNull DeleteInstanceResponse'

instance Hashable DeleteInstance

instance NFData DeleteInstance

instance ToHeaders DeleteInstance where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteInstance where
  toPath DeleteInstance' {..} =
    mconcat ["/instance/", toBS _dInstanceId]

instance ToQuery DeleteInstance where
  toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
deleteInstanceResponse ::
  DeleteInstanceResponse
deleteInstanceResponse = DeleteInstanceResponse'

instance NFData DeleteInstanceResponse
