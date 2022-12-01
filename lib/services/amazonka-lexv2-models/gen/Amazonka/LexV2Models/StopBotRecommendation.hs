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
-- Module      : Amazonka.LexV2Models.StopBotRecommendation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an already running Bot Recommendation request.
module Amazonka.LexV2Models.StopBotRecommendation
  ( -- * Creating a Request
    StopBotRecommendation (..),
    newStopBotRecommendation,

    -- * Request Lenses
    stopBotRecommendation_botId,
    stopBotRecommendation_botVersion,
    stopBotRecommendation_localeId,
    stopBotRecommendation_botRecommendationId,

    -- * Destructuring the Response
    StopBotRecommendationResponse (..),
    newStopBotRecommendationResponse,

    -- * Response Lenses
    stopBotRecommendationResponse_botVersion,
    stopBotRecommendationResponse_localeId,
    stopBotRecommendationResponse_botRecommendationId,
    stopBotRecommendationResponse_botId,
    stopBotRecommendationResponse_botRecommendationStatus,
    stopBotRecommendationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopBotRecommendation' smart constructor.
data StopBotRecommendation = StopBotRecommendation'
  { -- | The unique identifier of the bot containing the bot recommendation to be
    -- stopped.
    botId :: Prelude.Text,
    -- | The version of the bot containing the bot recommendation.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the bot recommendation to
    -- stop. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Text,
    -- | The unique identifier of the bot recommendation to be stopped.
    botRecommendationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBotRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'stopBotRecommendation_botId' - The unique identifier of the bot containing the bot recommendation to be
-- stopped.
--
-- 'botVersion', 'stopBotRecommendation_botVersion' - The version of the bot containing the bot recommendation.
--
-- 'localeId', 'stopBotRecommendation_localeId' - The identifier of the language and locale of the bot recommendation to
-- stop. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'botRecommendationId', 'stopBotRecommendation_botRecommendationId' - The unique identifier of the bot recommendation to be stopped.
newStopBotRecommendation ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'botRecommendationId'
  Prelude.Text ->
  StopBotRecommendation
newStopBotRecommendation
  pBotId_
  pBotVersion_
  pLocaleId_
  pBotRecommendationId_ =
    StopBotRecommendation'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        botRecommendationId = pBotRecommendationId_
      }

-- | The unique identifier of the bot containing the bot recommendation to be
-- stopped.
stopBotRecommendation_botId :: Lens.Lens' StopBotRecommendation Prelude.Text
stopBotRecommendation_botId = Lens.lens (\StopBotRecommendation' {botId} -> botId) (\s@StopBotRecommendation' {} a -> s {botId = a} :: StopBotRecommendation)

-- | The version of the bot containing the bot recommendation.
stopBotRecommendation_botVersion :: Lens.Lens' StopBotRecommendation Prelude.Text
stopBotRecommendation_botVersion = Lens.lens (\StopBotRecommendation' {botVersion} -> botVersion) (\s@StopBotRecommendation' {} a -> s {botVersion = a} :: StopBotRecommendation)

-- | The identifier of the language and locale of the bot recommendation to
-- stop. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
stopBotRecommendation_localeId :: Lens.Lens' StopBotRecommendation Prelude.Text
stopBotRecommendation_localeId = Lens.lens (\StopBotRecommendation' {localeId} -> localeId) (\s@StopBotRecommendation' {} a -> s {localeId = a} :: StopBotRecommendation)

-- | The unique identifier of the bot recommendation to be stopped.
stopBotRecommendation_botRecommendationId :: Lens.Lens' StopBotRecommendation Prelude.Text
stopBotRecommendation_botRecommendationId = Lens.lens (\StopBotRecommendation' {botRecommendationId} -> botRecommendationId) (\s@StopBotRecommendation' {} a -> s {botRecommendationId = a} :: StopBotRecommendation)

instance Core.AWSRequest StopBotRecommendation where
  type
    AWSResponse StopBotRecommendation =
      StopBotRecommendationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopBotRecommendationResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "botRecommendationId")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botRecommendationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopBotRecommendation where
  hashWithSalt _salt StopBotRecommendation' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` botRecommendationId

instance Prelude.NFData StopBotRecommendation where
  rnf StopBotRecommendation' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botRecommendationId

instance Core.ToHeaders StopBotRecommendation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopBotRecommendation where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StopBotRecommendation where
  toPath StopBotRecommendation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/botrecommendations/",
        Core.toBS botRecommendationId,
        "/stopbotrecommendation"
      ]

instance Core.ToQuery StopBotRecommendation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBotRecommendationResponse' smart constructor.
data StopBotRecommendationResponse = StopBotRecommendationResponse'
  { -- | The version of the bot containing the recommendation that is being
    -- stopped.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the language and locale of the bot response to stop.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot recommendation that is being stopped.
    botRecommendationId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot containing the bot recommendation that
    -- is being stopped.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The status of the bot recommendation. If the status is Failed, then the
    -- reasons for the failure are listed in the failureReasons field.
    botRecommendationStatus :: Prelude.Maybe BotRecommendationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBotRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botVersion', 'stopBotRecommendationResponse_botVersion' - The version of the bot containing the recommendation that is being
-- stopped.
--
-- 'localeId', 'stopBotRecommendationResponse_localeId' - The identifier of the language and locale of the bot response to stop.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'botRecommendationId', 'stopBotRecommendationResponse_botRecommendationId' - The unique identifier of the bot recommendation that is being stopped.
--
-- 'botId', 'stopBotRecommendationResponse_botId' - The unique identifier of the bot containing the bot recommendation that
-- is being stopped.
--
-- 'botRecommendationStatus', 'stopBotRecommendationResponse_botRecommendationStatus' - The status of the bot recommendation. If the status is Failed, then the
-- reasons for the failure are listed in the failureReasons field.
--
-- 'httpStatus', 'stopBotRecommendationResponse_httpStatus' - The response's http status code.
newStopBotRecommendationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopBotRecommendationResponse
newStopBotRecommendationResponse pHttpStatus_ =
  StopBotRecommendationResponse'
    { botVersion =
        Prelude.Nothing,
      localeId = Prelude.Nothing,
      botRecommendationId = Prelude.Nothing,
      botId = Prelude.Nothing,
      botRecommendationStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot containing the recommendation that is being
-- stopped.
stopBotRecommendationResponse_botVersion :: Lens.Lens' StopBotRecommendationResponse (Prelude.Maybe Prelude.Text)
stopBotRecommendationResponse_botVersion = Lens.lens (\StopBotRecommendationResponse' {botVersion} -> botVersion) (\s@StopBotRecommendationResponse' {} a -> s {botVersion = a} :: StopBotRecommendationResponse)

-- | The identifier of the language and locale of the bot response to stop.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
stopBotRecommendationResponse_localeId :: Lens.Lens' StopBotRecommendationResponse (Prelude.Maybe Prelude.Text)
stopBotRecommendationResponse_localeId = Lens.lens (\StopBotRecommendationResponse' {localeId} -> localeId) (\s@StopBotRecommendationResponse' {} a -> s {localeId = a} :: StopBotRecommendationResponse)

-- | The unique identifier of the bot recommendation that is being stopped.
stopBotRecommendationResponse_botRecommendationId :: Lens.Lens' StopBotRecommendationResponse (Prelude.Maybe Prelude.Text)
stopBotRecommendationResponse_botRecommendationId = Lens.lens (\StopBotRecommendationResponse' {botRecommendationId} -> botRecommendationId) (\s@StopBotRecommendationResponse' {} a -> s {botRecommendationId = a} :: StopBotRecommendationResponse)

-- | The unique identifier of the bot containing the bot recommendation that
-- is being stopped.
stopBotRecommendationResponse_botId :: Lens.Lens' StopBotRecommendationResponse (Prelude.Maybe Prelude.Text)
stopBotRecommendationResponse_botId = Lens.lens (\StopBotRecommendationResponse' {botId} -> botId) (\s@StopBotRecommendationResponse' {} a -> s {botId = a} :: StopBotRecommendationResponse)

-- | The status of the bot recommendation. If the status is Failed, then the
-- reasons for the failure are listed in the failureReasons field.
stopBotRecommendationResponse_botRecommendationStatus :: Lens.Lens' StopBotRecommendationResponse (Prelude.Maybe BotRecommendationStatus)
stopBotRecommendationResponse_botRecommendationStatus = Lens.lens (\StopBotRecommendationResponse' {botRecommendationStatus} -> botRecommendationStatus) (\s@StopBotRecommendationResponse' {} a -> s {botRecommendationStatus = a} :: StopBotRecommendationResponse)

-- | The response's http status code.
stopBotRecommendationResponse_httpStatus :: Lens.Lens' StopBotRecommendationResponse Prelude.Int
stopBotRecommendationResponse_httpStatus = Lens.lens (\StopBotRecommendationResponse' {httpStatus} -> httpStatus) (\s@StopBotRecommendationResponse' {} a -> s {httpStatus = a} :: StopBotRecommendationResponse)

instance Prelude.NFData StopBotRecommendationResponse where
  rnf StopBotRecommendationResponse' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botRecommendationId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botRecommendationStatus
      `Prelude.seq` Prelude.rnf httpStatus
