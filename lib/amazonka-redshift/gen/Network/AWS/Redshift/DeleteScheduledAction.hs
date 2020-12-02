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
-- Module      : Network.AWS.Redshift.DeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled action.
module Network.AWS.Redshift.DeleteScheduledAction
  ( -- * Creating a Request
    deleteScheduledAction,
    DeleteScheduledAction,

    -- * Request Lenses
    dsaScheduledActionName,

    -- * Destructuring the Response
    deleteScheduledActionResponse,
    DeleteScheduledActionResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScheduledAction' smart constructor.
newtype DeleteScheduledAction = DeleteScheduledAction'
  { _dsaScheduledActionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaScheduledActionName' - The name of the scheduled action to delete.
deleteScheduledAction ::
  -- | 'dsaScheduledActionName'
  Text ->
  DeleteScheduledAction
deleteScheduledAction pScheduledActionName_ =
  DeleteScheduledAction'
    { _dsaScheduledActionName =
        pScheduledActionName_
    }

-- | The name of the scheduled action to delete.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName = lens _dsaScheduledActionName (\s a -> s {_dsaScheduledActionName = a})

instance AWSRequest DeleteScheduledAction where
  type Rs DeleteScheduledAction = DeleteScheduledActionResponse
  request = postQuery redshift
  response = receiveNull DeleteScheduledActionResponse'

instance Hashable DeleteScheduledAction

instance NFData DeleteScheduledAction

instance ToHeaders DeleteScheduledAction where
  toHeaders = const mempty

instance ToPath DeleteScheduledAction where
  toPath = const "/"

instance ToQuery DeleteScheduledAction where
  toQuery DeleteScheduledAction' {..} =
    mconcat
      [ "Action" =: ("DeleteScheduledAction" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ScheduledActionName" =: _dsaScheduledActionName
      ]

-- | /See:/ 'deleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScheduledActionResponse' with the minimum fields required to make a request.
deleteScheduledActionResponse ::
  DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse'

instance NFData DeleteScheduledActionResponse
