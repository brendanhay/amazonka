{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- To delete a script, specify the script ID. Before deleting a script, be sure to terminate all fleets that are deployed with the script being deleted. Fleet instances periodically check for script updates, and if the script record no longer exists, the instance will go into an error state and be unable to host game sessions.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
-- __Related operations__
--
--     * 'CreateScript'
--
--
--     * 'ListScripts'
--
--
--     * 'DescribeScript'
--
--
--     * 'UpdateScript'
--
--
--     * 'DeleteScript'
module Network.AWS.GameLift.DeleteScript
  ( -- * Creating a request
    DeleteScript (..),
    mkDeleteScript,

    -- ** Request lenses
    dScriptId,

    -- * Destructuring the response
    DeleteScriptResponse (..),
    mkDeleteScriptResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteScript' smart constructor.
newtype DeleteScript = DeleteScript'
  { -- | A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
    scriptId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScript' with the minimum fields required to make a request.
--
-- * 'scriptId' - A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
mkDeleteScript ::
  -- | 'scriptId'
  Lude.Text ->
  DeleteScript
mkDeleteScript pScriptId_ = DeleteScript' {scriptId = pScriptId_}

-- | A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScriptId :: Lens.Lens' DeleteScript Lude.Text
dScriptId = Lens.lens (scriptId :: DeleteScript -> Lude.Text) (\s a -> s {scriptId = a} :: DeleteScript)
{-# DEPRECATED dScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

instance Lude.AWSRequest DeleteScript where
  type Rs DeleteScript = DeleteScriptResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeleteScriptResponse'

instance Lude.ToHeaders DeleteScript where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteScript" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteScript where
  toJSON DeleteScript' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ScriptId" Lude..= scriptId)])

instance Lude.ToPath DeleteScript where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteScript where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteScriptResponse' smart constructor.
data DeleteScriptResponse = DeleteScriptResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScriptResponse' with the minimum fields required to make a request.
mkDeleteScriptResponse ::
  DeleteScriptResponse
mkDeleteScriptResponse = DeleteScriptResponse'
