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
-- Module      : Network.AWS.GameLift.GetGameSessionLogUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the location of stored game session logs for a specified game
-- session. When a game session is terminated, Amazon GameLift
-- automatically stores the logs in Amazon S3 and retains them for 14 days.
-- Use this URL to download the logs.
--
-- See the
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift AWS Service Limits>
-- page for maximum log file sizes. Log files that exceed this limit are
-- not saved.
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
module Network.AWS.GameLift.GetGameSessionLogUrl
  ( -- * Creating a Request
    GetGameSessionLogUrl (..),
    newGetGameSessionLogUrl,

    -- * Request Lenses
    getGameSessionLogUrl_gameSessionId,

    -- * Destructuring the Response
    GetGameSessionLogUrlResponse (..),
    newGetGameSessionLogUrlResponse,

    -- * Response Lenses
    getGameSessionLogUrlResponse_preSignedUrl,
    getGameSessionLogUrlResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newGetGameSessionLogUrl' smart constructor.
data GetGameSessionLogUrl = GetGameSessionLogUrl'
  { -- | A unique identifier for the game session to get logs for.
    gameSessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGameSessionLogUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionId', 'getGameSessionLogUrl_gameSessionId' - A unique identifier for the game session to get logs for.
newGetGameSessionLogUrl ::
  -- | 'gameSessionId'
  Core.Text ->
  GetGameSessionLogUrl
newGetGameSessionLogUrl pGameSessionId_ =
  GetGameSessionLogUrl'
    { gameSessionId =
        pGameSessionId_
    }

-- | A unique identifier for the game session to get logs for.
getGameSessionLogUrl_gameSessionId :: Lens.Lens' GetGameSessionLogUrl Core.Text
getGameSessionLogUrl_gameSessionId = Lens.lens (\GetGameSessionLogUrl' {gameSessionId} -> gameSessionId) (\s@GetGameSessionLogUrl' {} a -> s {gameSessionId = a} :: GetGameSessionLogUrl)

instance Core.AWSRequest GetGameSessionLogUrl where
  type
    AWSResponse GetGameSessionLogUrl =
      GetGameSessionLogUrlResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGameSessionLogUrlResponse'
            Core.<$> (x Core..?> "PreSignedUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGameSessionLogUrl

instance Core.NFData GetGameSessionLogUrl

instance Core.ToHeaders GetGameSessionLogUrl where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.GetGameSessionLogUrl" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetGameSessionLogUrl where
  toJSON GetGameSessionLogUrl' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GameSessionId" Core..= gameSessionId)]
      )

instance Core.ToPath GetGameSessionLogUrl where
  toPath = Core.const "/"

instance Core.ToQuery GetGameSessionLogUrl where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newGetGameSessionLogUrlResponse' smart constructor.
data GetGameSessionLogUrlResponse = GetGameSessionLogUrlResponse'
  { -- | Location of the requested game session logs, available for download.
    -- This URL is valid for 15 minutes, after which S3 will reject any
    -- download request using this URL. You can request a new URL any time
    -- within the 14-day period that the logs are retained.
    preSignedUrl :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGameSessionLogUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preSignedUrl', 'getGameSessionLogUrlResponse_preSignedUrl' - Location of the requested game session logs, available for download.
-- This URL is valid for 15 minutes, after which S3 will reject any
-- download request using this URL. You can request a new URL any time
-- within the 14-day period that the logs are retained.
--
-- 'httpStatus', 'getGameSessionLogUrlResponse_httpStatus' - The response's http status code.
newGetGameSessionLogUrlResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGameSessionLogUrlResponse
newGetGameSessionLogUrlResponse pHttpStatus_ =
  GetGameSessionLogUrlResponse'
    { preSignedUrl =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Location of the requested game session logs, available for download.
-- This URL is valid for 15 minutes, after which S3 will reject any
-- download request using this URL. You can request a new URL any time
-- within the 14-day period that the logs are retained.
getGameSessionLogUrlResponse_preSignedUrl :: Lens.Lens' GetGameSessionLogUrlResponse (Core.Maybe Core.Text)
getGameSessionLogUrlResponse_preSignedUrl = Lens.lens (\GetGameSessionLogUrlResponse' {preSignedUrl} -> preSignedUrl) (\s@GetGameSessionLogUrlResponse' {} a -> s {preSignedUrl = a} :: GetGameSessionLogUrlResponse)

-- | The response's http status code.
getGameSessionLogUrlResponse_httpStatus :: Lens.Lens' GetGameSessionLogUrlResponse Core.Int
getGameSessionLogUrlResponse_httpStatus = Lens.lens (\GetGameSessionLogUrlResponse' {httpStatus} -> httpStatus) (\s@GetGameSessionLogUrlResponse' {} a -> s {httpStatus = a} :: GetGameSessionLogUrlResponse)

instance Core.NFData GetGameSessionLogUrlResponse
