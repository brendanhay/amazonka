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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newGetGameSessionLogUrl' smart constructor.
data GetGameSessionLogUrl = GetGameSessionLogUrl'
  { -- | A unique identifier for the game session to get logs for.
    gameSessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetGameSessionLogUrl
newGetGameSessionLogUrl pGameSessionId_ =
  GetGameSessionLogUrl'
    { gameSessionId =
        pGameSessionId_
    }

-- | A unique identifier for the game session to get logs for.
getGameSessionLogUrl_gameSessionId :: Lens.Lens' GetGameSessionLogUrl Prelude.Text
getGameSessionLogUrl_gameSessionId = Lens.lens (\GetGameSessionLogUrl' {gameSessionId} -> gameSessionId) (\s@GetGameSessionLogUrl' {} a -> s {gameSessionId = a} :: GetGameSessionLogUrl)

instance Prelude.AWSRequest GetGameSessionLogUrl where
  type
    Rs GetGameSessionLogUrl =
      GetGameSessionLogUrlResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGameSessionLogUrlResponse'
            Prelude.<$> (x Prelude..?> "PreSignedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGameSessionLogUrl

instance Prelude.NFData GetGameSessionLogUrl

instance Prelude.ToHeaders GetGameSessionLogUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.GetGameSessionLogUrl" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetGameSessionLogUrl where
  toJSON GetGameSessionLogUrl' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameSessionId" Prelude..= gameSessionId)
          ]
      )

instance Prelude.ToPath GetGameSessionLogUrl where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetGameSessionLogUrl where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newGetGameSessionLogUrlResponse' smart constructor.
data GetGameSessionLogUrlResponse = GetGameSessionLogUrlResponse'
  { -- | Location of the requested game session logs, available for download.
    -- This URL is valid for 15 minutes, after which S3 will reject any
    -- download request using this URL. You can request a new URL any time
    -- within the 14-day period that the logs are retained.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetGameSessionLogUrlResponse
newGetGameSessionLogUrlResponse pHttpStatus_ =
  GetGameSessionLogUrlResponse'
    { preSignedUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Location of the requested game session logs, available for download.
-- This URL is valid for 15 minutes, after which S3 will reject any
-- download request using this URL. You can request a new URL any time
-- within the 14-day period that the logs are retained.
getGameSessionLogUrlResponse_preSignedUrl :: Lens.Lens' GetGameSessionLogUrlResponse (Prelude.Maybe Prelude.Text)
getGameSessionLogUrlResponse_preSignedUrl = Lens.lens (\GetGameSessionLogUrlResponse' {preSignedUrl} -> preSignedUrl) (\s@GetGameSessionLogUrlResponse' {} a -> s {preSignedUrl = a} :: GetGameSessionLogUrlResponse)

-- | The response's http status code.
getGameSessionLogUrlResponse_httpStatus :: Lens.Lens' GetGameSessionLogUrlResponse Prelude.Int
getGameSessionLogUrlResponse_httpStatus = Lens.lens (\GetGameSessionLogUrlResponse' {httpStatus} -> httpStatus) (\s@GetGameSessionLogUrlResponse' {} a -> s {httpStatus = a} :: GetGameSessionLogUrlResponse)

instance Prelude.NFData GetGameSessionLogUrlResponse
