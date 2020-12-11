{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A session is an instance of a browser created through a @RemoteWebDriver@ with the URL from 'CreateTestGridUrlResult$url' . You can use the following to look up sessions:
--
--
--     * The session ARN ('GetTestGridSessionRequest$sessionArn' ).
--
--
--     * The project ARN and a session ID ('GetTestGridSessionRequest$projectArn' and 'GetTestGridSessionRequest$sessionId' ).
module Network.AWS.DeviceFarm.GetTestGridSession
  ( -- * Creating a request
    GetTestGridSession (..),
    mkGetTestGridSession,

    -- ** Request lenses
    gtgsSessionARN,
    gtgsProjectARN,
    gtgsSessionId,

    -- * Destructuring the response
    GetTestGridSessionResponse (..),
    mkGetTestGridSessionResponse,

    -- ** Response lenses
    gtgsrsTestGridSession,
    gtgsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { sessionARN ::
      Lude.Maybe Lude.Text,
    projectARN :: Lude.Maybe Lude.Text,
    sessionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTestGridSession' with the minimum fields required to make a request.
--
-- * 'projectARN' - The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
-- * 'sessionARN' - An ARN that uniquely identifies a 'TestGridSession' .
-- * 'sessionId' - An ID associated with this session.
mkGetTestGridSession ::
  GetTestGridSession
mkGetTestGridSession =
  GetTestGridSession'
    { sessionARN = Lude.Nothing,
      projectARN = Lude.Nothing,
      sessionId = Lude.Nothing
    }

-- | An ARN that uniquely identifies a 'TestGridSession' .
--
-- /Note:/ Consider using 'sessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionARN :: Lens.Lens' GetTestGridSession (Lude.Maybe Lude.Text)
gtgsSessionARN = Lens.lens (sessionARN :: GetTestGridSession -> Lude.Maybe Lude.Text) (\s a -> s {sessionARN = a} :: GetTestGridSession)
{-# DEPRECATED gtgsSessionARN "Use generic-lens or generic-optics with 'sessionARN' instead." #-}

-- | The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsProjectARN :: Lens.Lens' GetTestGridSession (Lude.Maybe Lude.Text)
gtgsProjectARN = Lens.lens (projectARN :: GetTestGridSession -> Lude.Maybe Lude.Text) (\s a -> s {projectARN = a} :: GetTestGridSession)
{-# DEPRECATED gtgsProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | An ID associated with this session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionId :: Lens.Lens' GetTestGridSession (Lude.Maybe Lude.Text)
gtgsSessionId = Lens.lens (sessionId :: GetTestGridSession -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: GetTestGridSession)
{-# DEPRECATED gtgsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Lude.AWSRequest GetTestGridSession where
  type Rs GetTestGridSession = GetTestGridSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            Lude.<$> (x Lude..?> "testGridSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTestGridSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetTestGridSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTestGridSession where
  toJSON GetTestGridSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sessionArn" Lude..=) Lude.<$> sessionARN,
            ("projectArn" Lude..=) Lude.<$> projectARN,
            ("sessionId" Lude..=) Lude.<$> sessionId
          ]
      )

instance Lude.ToPath GetTestGridSession where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTestGridSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { testGridSession ::
      Lude.Maybe TestGridSession,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTestGridSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'testGridSession' - The 'TestGridSession' that was requested.
mkGetTestGridSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTestGridSessionResponse
mkGetTestGridSessionResponse pResponseStatus_ =
  GetTestGridSessionResponse'
    { testGridSession = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'TestGridSession' that was requested.
--
-- /Note:/ Consider using 'testGridSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrsTestGridSession :: Lens.Lens' GetTestGridSessionResponse (Lude.Maybe TestGridSession)
gtgsrsTestGridSession = Lens.lens (testGridSession :: GetTestGridSessionResponse -> Lude.Maybe TestGridSession) (\s a -> s {testGridSession = a} :: GetTestGridSessionResponse)
{-# DEPRECATED gtgsrsTestGridSession "Use generic-lens or generic-optics with 'testGridSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrsResponseStatus :: Lens.Lens' GetTestGridSessionResponse Lude.Int
gtgsrsResponseStatus = Lens.lens (responseStatus :: GetTestGridSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTestGridSessionResponse)
{-# DEPRECATED gtgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
