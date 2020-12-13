{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a Realtime script.
--
-- To request a script record, specify the script ID. If successful, an object containing the script properties is returned.
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
module Network.AWS.GameLift.DescribeScript
  ( -- * Creating a request
    DescribeScript (..),
    mkDescribeScript,

    -- ** Request lenses
    dsScriptId,

    -- * Destructuring the response
    DescribeScriptResponse (..),
    mkDescribeScriptResponse,

    -- ** Response lenses
    dsrsScript,
    dsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScript' smart constructor.
newtype DescribeScript = DescribeScript'
  { -- | A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
    scriptId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScript' with the minimum fields required to make a request.
--
-- * 'scriptId' - A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
mkDescribeScript ::
  -- | 'scriptId'
  Lude.Text ->
  DescribeScript
mkDescribeScript pScriptId_ =
  DescribeScript' {scriptId = pScriptId_}

-- | A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsScriptId :: Lens.Lens' DescribeScript Lude.Text
dsScriptId = Lens.lens (scriptId :: DescribeScript -> Lude.Text) (\s a -> s {scriptId = a} :: DescribeScript)
{-# DEPRECATED dsScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

instance Lude.AWSRequest DescribeScript where
  type Rs DescribeScript = DescribeScriptResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScriptResponse'
            Lude.<$> (x Lude..?> "Script") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScript where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeScript" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScript where
  toJSON DescribeScript' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ScriptId" Lude..= scriptId)])

instance Lude.ToPath DescribeScript where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScript where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScriptResponse' smart constructor.
data DescribeScriptResponse = DescribeScriptResponse'
  { -- | A set of properties describing the requested script.
    script :: Lude.Maybe Script,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScriptResponse' with the minimum fields required to make a request.
--
-- * 'script' - A set of properties describing the requested script.
-- * 'responseStatus' - The response status code.
mkDescribeScriptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScriptResponse
mkDescribeScriptResponse pResponseStatus_ =
  DescribeScriptResponse'
    { script = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of properties describing the requested script.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsScript :: Lens.Lens' DescribeScriptResponse (Lude.Maybe Script)
dsrsScript = Lens.lens (script :: DescribeScriptResponse -> Lude.Maybe Script) (\s a -> s {script = a} :: DescribeScriptResponse)
{-# DEPRECATED dsrsScript "Use generic-lens or generic-optics with 'script' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeScriptResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeScriptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScriptResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
