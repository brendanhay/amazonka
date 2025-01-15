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
-- Module      : Amazonka.LexV2Models.BuildBotLocale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Builds a bot, its intents, and its slot types into a specific locale. A
-- bot can be built into multiple locales. At runtime the locale is used to
-- choose a specific build of the bot.
module Amazonka.LexV2Models.BuildBotLocale
  ( -- * Creating a Request
    BuildBotLocale (..),
    newBuildBotLocale,

    -- * Request Lenses
    buildBotLocale_botId,
    buildBotLocale_botVersion,
    buildBotLocale_localeId,

    -- * Destructuring the Response
    BuildBotLocaleResponse (..),
    newBuildBotLocaleResponse,

    -- * Response Lenses
    buildBotLocaleResponse_botId,
    buildBotLocaleResponse_botLocaleStatus,
    buildBotLocaleResponse_botVersion,
    buildBotLocaleResponse_lastBuildSubmittedDateTime,
    buildBotLocaleResponse_localeId,
    buildBotLocaleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBuildBotLocale' smart constructor.
data BuildBotLocale = BuildBotLocale'
  { -- | The identifier of the bot to build. The identifier is returned in the
    -- response from the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_CreateBot.html CreateBot>
    -- operation.
    botId :: Prelude.Text,
    -- | The version of the bot to build. This can only be the draft version of
    -- the bot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the bot will be used in.
    -- The string must match one of the supported locales. All of the intents,
    -- slot types, and slots used in the bot must have the same locale. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildBotLocale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'buildBotLocale_botId' - The identifier of the bot to build. The identifier is returned in the
-- response from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_CreateBot.html CreateBot>
-- operation.
--
-- 'botVersion', 'buildBotLocale_botVersion' - The version of the bot to build. This can only be the draft version of
-- the bot.
--
-- 'localeId', 'buildBotLocale_localeId' - The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newBuildBotLocale ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  BuildBotLocale
newBuildBotLocale pBotId_ pBotVersion_ pLocaleId_ =
  BuildBotLocale'
    { botId = pBotId_,
      botVersion = pBotVersion_,
      localeId = pLocaleId_
    }

-- | The identifier of the bot to build. The identifier is returned in the
-- response from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_CreateBot.html CreateBot>
-- operation.
buildBotLocale_botId :: Lens.Lens' BuildBotLocale Prelude.Text
buildBotLocale_botId = Lens.lens (\BuildBotLocale' {botId} -> botId) (\s@BuildBotLocale' {} a -> s {botId = a} :: BuildBotLocale)

-- | The version of the bot to build. This can only be the draft version of
-- the bot.
buildBotLocale_botVersion :: Lens.Lens' BuildBotLocale Prelude.Text
buildBotLocale_botVersion = Lens.lens (\BuildBotLocale' {botVersion} -> botVersion) (\s@BuildBotLocale' {} a -> s {botVersion = a} :: BuildBotLocale)

-- | The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
buildBotLocale_localeId :: Lens.Lens' BuildBotLocale Prelude.Text
buildBotLocale_localeId = Lens.lens (\BuildBotLocale' {localeId} -> localeId) (\s@BuildBotLocale' {} a -> s {localeId = a} :: BuildBotLocale)

instance Core.AWSRequest BuildBotLocale where
  type
    AWSResponse BuildBotLocale =
      BuildBotLocaleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BuildBotLocaleResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botLocaleStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "lastBuildSubmittedDateTime")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BuildBotLocale where
  hashWithSalt _salt BuildBotLocale' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData BuildBotLocale where
  rnf BuildBotLocale' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf localeId

instance Data.ToHeaders BuildBotLocale where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BuildBotLocale where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath BuildBotLocale where
  toPath BuildBotLocale' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/"
      ]

instance Data.ToQuery BuildBotLocale where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBuildBotLocaleResponse' smart constructor.
data BuildBotLocaleResponse = BuildBotLocaleResponse'
  { -- | The identifier of the specified bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The bot\'s build status. When the status is @ReadyExpressTesting@ you
    -- can test the bot using the utterances defined for the intents and slot
    -- types. When the status is @Built@, the bot is ready for use and can be
    -- tested using any utterance.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | The version of the bot that was built. This is only the draft version of
    -- the bot.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp indicating the date and time that the bot was last built for
    -- this locale.
    lastBuildSubmittedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The language and locale specified of where the bot can be used.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildBotLocaleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'buildBotLocaleResponse_botId' - The identifier of the specified bot.
--
-- 'botLocaleStatus', 'buildBotLocaleResponse_botLocaleStatus' - The bot\'s build status. When the status is @ReadyExpressTesting@ you
-- can test the bot using the utterances defined for the intents and slot
-- types. When the status is @Built@, the bot is ready for use and can be
-- tested using any utterance.
--
-- 'botVersion', 'buildBotLocaleResponse_botVersion' - The version of the bot that was built. This is only the draft version of
-- the bot.
--
-- 'lastBuildSubmittedDateTime', 'buildBotLocaleResponse_lastBuildSubmittedDateTime' - A timestamp indicating the date and time that the bot was last built for
-- this locale.
--
-- 'localeId', 'buildBotLocaleResponse_localeId' - The language and locale specified of where the bot can be used.
--
-- 'httpStatus', 'buildBotLocaleResponse_httpStatus' - The response's http status code.
newBuildBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BuildBotLocaleResponse
newBuildBotLocaleResponse pHttpStatus_ =
  BuildBotLocaleResponse'
    { botId = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      lastBuildSubmittedDateTime = Prelude.Nothing,
      localeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the specified bot.
buildBotLocaleResponse_botId :: Lens.Lens' BuildBotLocaleResponse (Prelude.Maybe Prelude.Text)
buildBotLocaleResponse_botId = Lens.lens (\BuildBotLocaleResponse' {botId} -> botId) (\s@BuildBotLocaleResponse' {} a -> s {botId = a} :: BuildBotLocaleResponse)

-- | The bot\'s build status. When the status is @ReadyExpressTesting@ you
-- can test the bot using the utterances defined for the intents and slot
-- types. When the status is @Built@, the bot is ready for use and can be
-- tested using any utterance.
buildBotLocaleResponse_botLocaleStatus :: Lens.Lens' BuildBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
buildBotLocaleResponse_botLocaleStatus = Lens.lens (\BuildBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@BuildBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: BuildBotLocaleResponse)

-- | The version of the bot that was built. This is only the draft version of
-- the bot.
buildBotLocaleResponse_botVersion :: Lens.Lens' BuildBotLocaleResponse (Prelude.Maybe Prelude.Text)
buildBotLocaleResponse_botVersion = Lens.lens (\BuildBotLocaleResponse' {botVersion} -> botVersion) (\s@BuildBotLocaleResponse' {} a -> s {botVersion = a} :: BuildBotLocaleResponse)

-- | A timestamp indicating the date and time that the bot was last built for
-- this locale.
buildBotLocaleResponse_lastBuildSubmittedDateTime :: Lens.Lens' BuildBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
buildBotLocaleResponse_lastBuildSubmittedDateTime = Lens.lens (\BuildBotLocaleResponse' {lastBuildSubmittedDateTime} -> lastBuildSubmittedDateTime) (\s@BuildBotLocaleResponse' {} a -> s {lastBuildSubmittedDateTime = a} :: BuildBotLocaleResponse) Prelude.. Lens.mapping Data._Time

-- | The language and locale specified of where the bot can be used.
buildBotLocaleResponse_localeId :: Lens.Lens' BuildBotLocaleResponse (Prelude.Maybe Prelude.Text)
buildBotLocaleResponse_localeId = Lens.lens (\BuildBotLocaleResponse' {localeId} -> localeId) (\s@BuildBotLocaleResponse' {} a -> s {localeId = a} :: BuildBotLocaleResponse)

-- | The response's http status code.
buildBotLocaleResponse_httpStatus :: Lens.Lens' BuildBotLocaleResponse Prelude.Int
buildBotLocaleResponse_httpStatus = Lens.lens (\BuildBotLocaleResponse' {httpStatus} -> httpStatus) (\s@BuildBotLocaleResponse' {} a -> s {httpStatus = a} :: BuildBotLocaleResponse)

instance Prelude.NFData BuildBotLocaleResponse where
  rnf BuildBotLocaleResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botLocaleStatus `Prelude.seq`
        Prelude.rnf botVersion `Prelude.seq`
          Prelude.rnf lastBuildSubmittedDateTime `Prelude.seq`
            Prelude.rnf localeId `Prelude.seq`
              Prelude.rnf httpStatus
