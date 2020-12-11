{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.GetGameSessionLogURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the location of stored game session logs for a specified game session. When a game session is terminated, Amazon GameLift automatically stores the logs in Amazon S3 and retains them for 14 days. Use this URL to download the logs.
--
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
module Network.AWS.GameLift.GetGameSessionLogURL
  ( -- * Creating a request
    GetGameSessionLogURL (..),
    mkGetGameSessionLogURL,

    -- ** Request lenses
    ggsluGameSessionId,

    -- * Destructuring the response
    GetGameSessionLogURLResponse (..),
    mkGetGameSessionLogURLResponse,

    -- ** Response lenses
    ggslursPreSignedURL,
    ggslursResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkGetGameSessionLogURL' smart constructor.
newtype GetGameSessionLogURL = GetGameSessionLogURL'
  { gameSessionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGameSessionLogURL' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to get logs for.
mkGetGameSessionLogURL ::
  -- | 'gameSessionId'
  Lude.Text ->
  GetGameSessionLogURL
mkGetGameSessionLogURL pGameSessionId_ =
  GetGameSessionLogURL' {gameSessionId = pGameSessionId_}

-- | A unique identifier for the game session to get logs for.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsluGameSessionId :: Lens.Lens' GetGameSessionLogURL Lude.Text
ggsluGameSessionId = Lens.lens (gameSessionId :: GetGameSessionLogURL -> Lude.Text) (\s a -> s {gameSessionId = a} :: GetGameSessionLogURL)
{-# DEPRECATED ggsluGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

instance Lude.AWSRequest GetGameSessionLogURL where
  type Rs GetGameSessionLogURL = GetGameSessionLogURLResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGameSessionLogURLResponse'
            Lude.<$> (x Lude..?> "PreSignedUrl") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGameSessionLogURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.GetGameSessionLogUrl" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetGameSessionLogURL where
  toJSON GetGameSessionLogURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GameSessionId" Lude..= gameSessionId)]
      )

instance Lude.ToPath GetGameSessionLogURL where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGameSessionLogURL where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkGetGameSessionLogURLResponse' smart constructor.
data GetGameSessionLogURLResponse = GetGameSessionLogURLResponse'
  { preSignedURL ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetGameSessionLogURLResponse' with the minimum fields required to make a request.
--
-- * 'preSignedURL' - Location of the requested game session logs, available for download. This URL is valid for 15 minutes, after which S3 will reject any download request using this URL. You can request a new URL any time within the 14-day period that the logs are retained.
-- * 'responseStatus' - The response status code.
mkGetGameSessionLogURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGameSessionLogURLResponse
mkGetGameSessionLogURLResponse pResponseStatus_ =
  GetGameSessionLogURLResponse'
    { preSignedURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Location of the requested game session logs, available for download. This URL is valid for 15 minutes, after which S3 will reject any download request using this URL. You can request a new URL any time within the 14-day period that the logs are retained.
--
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggslursPreSignedURL :: Lens.Lens' GetGameSessionLogURLResponse (Lude.Maybe Lude.Text)
ggslursPreSignedURL = Lens.lens (preSignedURL :: GetGameSessionLogURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: GetGameSessionLogURLResponse)
{-# DEPRECATED ggslursPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggslursResponseStatus :: Lens.Lens' GetGameSessionLogURLResponse Lude.Int
ggslursResponseStatus = Lens.lens (responseStatus :: GetGameSessionLogURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGameSessionLogURLResponse)
{-# DEPRECATED ggslursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
