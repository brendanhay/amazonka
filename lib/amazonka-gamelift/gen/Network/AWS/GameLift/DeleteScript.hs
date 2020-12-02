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
-- Module      : Network.AWS.GameLift.DeleteScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Realtime script. This operation permanently deletes the script record. If script files were uploaded, they are also deleted (files stored in an S3 bucket are not deleted).
--
--
-- To delete a script, specify the script ID. Before deleting a script, be sure to terminate all fleets that are deployed with the script being deleted. Fleet instances periodically check for script updates, and if the script record no longer exists, the instance will go into an error state and be unable to host game sessions.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
--     * 'CreateScript'
--
--     * 'ListScripts'
--
--     * 'DescribeScript'
--
--     * 'UpdateScript'
--
--     * 'DeleteScript'
module Network.AWS.GameLift.DeleteScript
  ( -- * Creating a Request
    deleteScript,
    DeleteScript,

    -- * Request Lenses
    dsScriptId,

    -- * Destructuring the Response
    deleteScriptResponse,
    DeleteScriptResponse,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScript' smart constructor.
newtype DeleteScript = DeleteScript' {_dsScriptId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsScriptId' - A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
deleteScript ::
  -- | 'dsScriptId'
  Text ->
  DeleteScript
deleteScript pScriptId_ = DeleteScript' {_dsScriptId = pScriptId_}

-- | A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
dsScriptId :: Lens' DeleteScript Text
dsScriptId = lens _dsScriptId (\s a -> s {_dsScriptId = a})

instance AWSRequest DeleteScript where
  type Rs DeleteScript = DeleteScriptResponse
  request = postJSON gameLift
  response = receiveNull DeleteScriptResponse'

instance Hashable DeleteScript

instance NFData DeleteScript

instance ToHeaders DeleteScript where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DeleteScript" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteScript where
  toJSON DeleteScript' {..} =
    object (catMaybes [Just ("ScriptId" .= _dsScriptId)])

instance ToPath DeleteScript where
  toPath = const "/"

instance ToQuery DeleteScript where
  toQuery = const mempty

-- | /See:/ 'deleteScriptResponse' smart constructor.
data DeleteScriptResponse = DeleteScriptResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScriptResponse' with the minimum fields required to make a request.
deleteScriptResponse ::
  DeleteScriptResponse
deleteScriptResponse = DeleteScriptResponse'

instance NFData DeleteScriptResponse
