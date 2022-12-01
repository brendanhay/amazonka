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
-- Module      : Amazonka.GameLift.GetGameSessionLogUrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the location of stored game session logs for a specified game
-- session. When a game session is terminated, GameLift automatically
-- stores the logs in Amazon S3 and retains them for 14 days. Use this URL
-- to download the logs.
--
-- See the
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift Amazon Web Services Service Limits>
-- page for maximum log file sizes. Log files that exceed this limit are
-- not saved.
--
-- __Related actions__
--
-- CreateGameSession | DescribeGameSessions | DescribeGameSessionDetails |
-- SearchGameSessions | UpdateGameSession | GetGameSessionLogUrl |
-- StartGameSessionPlacement | DescribeGameSessionPlacement |
-- StopGameSessionPlacement |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.GetGameSessionLogUrl
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newGetGameSessionLogUrl' smart constructor.
data GetGameSessionLogUrl = GetGameSessionLogUrl'
  { -- | A unique identifier for the game session to get logs for.
    gameSessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetGameSessionLogUrl where
  type
    AWSResponse GetGameSessionLogUrl =
      GetGameSessionLogUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGameSessionLogUrlResponse'
            Prelude.<$> (x Core..?> "PreSignedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGameSessionLogUrl where
  hashWithSalt _salt GetGameSessionLogUrl' {..} =
    _salt `Prelude.hashWithSalt` gameSessionId

instance Prelude.NFData GetGameSessionLogUrl where
  rnf GetGameSessionLogUrl' {..} =
    Prelude.rnf gameSessionId

instance Core.ToHeaders GetGameSessionLogUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.GetGameSessionLogUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetGameSessionLogUrl where
  toJSON GetGameSessionLogUrl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameSessionId" Core..= gameSessionId)
          ]
      )

instance Core.ToPath GetGameSessionLogUrl where
  toPath = Prelude.const "/"

instance Core.ToQuery GetGameSessionLogUrl where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData GetGameSessionLogUrlResponse where
  rnf GetGameSessionLogUrlResponse' {..} =
    Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf httpStatus
