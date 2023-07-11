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
-- Module      : Amazonka.GameLift.DeleteScript
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteScript
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScript' smart constructor.
data DeleteScript = DeleteScript'
  { -- | A unique identifier for the Realtime script to delete. You can use
    -- either the script ID or ARN value.
    scriptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptId', 'deleteScript_scriptId' - A unique identifier for the Realtime script to delete. You can use
-- either the script ID or ARN value.
newDeleteScript ::
  -- | 'scriptId'
  Prelude.Text ->
  DeleteScript
newDeleteScript pScriptId_ =
  DeleteScript' {scriptId = pScriptId_}

-- | A unique identifier for the Realtime script to delete. You can use
-- either the script ID or ARN value.
deleteScript_scriptId :: Lens.Lens' DeleteScript Prelude.Text
deleteScript_scriptId = Lens.lens (\DeleteScript' {scriptId} -> scriptId) (\s@DeleteScript' {} a -> s {scriptId = a} :: DeleteScript)

instance Core.AWSRequest DeleteScript where
  type AWSResponse DeleteScript = DeleteScriptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteScriptResponse'

instance Prelude.Hashable DeleteScript where
  hashWithSalt _salt DeleteScript' {..} =
    _salt `Prelude.hashWithSalt` scriptId

instance Prelude.NFData DeleteScript where
  rnf DeleteScript' {..} = Prelude.rnf scriptId

instance Data.ToHeaders DeleteScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeleteScript" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteScript where
  toJSON DeleteScript' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ScriptId" Data..= scriptId)]
      )

instance Data.ToPath DeleteScript where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteScript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScriptResponse' smart constructor.
data DeleteScriptResponse = DeleteScriptResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScriptResponse ::
  DeleteScriptResponse
newDeleteScriptResponse = DeleteScriptResponse'

instance Prelude.NFData DeleteScriptResponse where
  rnf _ = ()
