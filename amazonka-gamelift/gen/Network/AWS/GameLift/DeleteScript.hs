{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Realtime script. This operation permanently deletes the script
-- record. If script files were uploaded, they are also deleted (files
-- stored in an S3 bucket are not deleted).
--
-- To delete a script, specify the script ID. Before deleting a script, be
-- sure to terminate all fleets that are deployed with the script being
-- deleted. Fleet instances periodically check for script updates, and if
-- the script record no longer exists, the instance will go into an error
-- state and be unable to host game sessions.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
-- -   CreateScript
--
-- -   ListScripts
--
-- -   DescribeScript
--
-- -   UpdateScript
--
-- -   DeleteScript
module Network.AWS.GameLift.DeleteScript
  ( -- * Creating a Request
    DeleteScript (..),
    newDeleteScript,

    -- * Request Lenses
    deleteScript_scriptId,

    -- * Destructuring the Response
    DeleteScriptResponse (..),
    newDeleteScriptResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteScript' smart constructor.
data DeleteScript = DeleteScript'
  { -- | A unique identifier for a Realtime script to delete. You can use either
    -- the script ID or ARN value.
    scriptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptId', 'deleteScript_scriptId' - A unique identifier for a Realtime script to delete. You can use either
-- the script ID or ARN value.
newDeleteScript ::
  -- | 'scriptId'
  Prelude.Text ->
  DeleteScript
newDeleteScript pScriptId_ =
  DeleteScript' {scriptId = pScriptId_}

-- | A unique identifier for a Realtime script to delete. You can use either
-- the script ID or ARN value.
deleteScript_scriptId :: Lens.Lens' DeleteScript Prelude.Text
deleteScript_scriptId = Lens.lens (\DeleteScript' {scriptId} -> scriptId) (\s@DeleteScript' {} a -> s {scriptId = a} :: DeleteScript)

instance Prelude.AWSRequest DeleteScript where
  type Rs DeleteScript = DeleteScriptResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteScriptResponse'

instance Prelude.Hashable DeleteScript

instance Prelude.NFData DeleteScript

instance Prelude.ToHeaders DeleteScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.DeleteScript" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteScript where
  toJSON DeleteScript' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ScriptId" Prelude..= scriptId)]
      )

instance Prelude.ToPath DeleteScript where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteScript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScriptResponse' smart constructor.
data DeleteScriptResponse = DeleteScriptResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScriptResponse ::
  DeleteScriptResponse
newDeleteScriptResponse = DeleteScriptResponse'

instance Prelude.NFData DeleteScriptResponse
