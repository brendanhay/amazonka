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
-- Module      : Network.AWS.LexV2Models.DeleteBotLocale
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a locale from a bot.
--
-- When you delete a locale, all intents, slots, and slot types defined for
-- the locale are also deleted.
module Network.AWS.LexV2Models.DeleteBotLocale
  ( -- * Creating a Request
    DeleteBotLocale (..),
    newDeleteBotLocale,

    -- * Request Lenses
    deleteBotLocale_botId,
    deleteBotLocale_botVersion,
    deleteBotLocale_localeId,

    -- * Destructuring the Response
    DeleteBotLocaleResponse (..),
    newDeleteBotLocaleResponse,

    -- * Response Lenses
    deleteBotLocaleResponse_botLocaleStatus,
    deleteBotLocaleResponse_botVersion,
    deleteBotLocaleResponse_botId,
    deleteBotLocaleResponse_localeId,
    deleteBotLocaleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBotLocale' smart constructor.
data DeleteBotLocale = DeleteBotLocale'
  { -- | The unique identifier of the bot that contains the locale.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the locale.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that will be deleted. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotLocale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteBotLocale_botId' - The unique identifier of the bot that contains the locale.
--
-- 'botVersion', 'deleteBotLocale_botVersion' - The version of the bot that contains the locale.
--
-- 'localeId', 'deleteBotLocale_localeId' - The identifier of the language and locale that will be deleted. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newDeleteBotLocale ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DeleteBotLocale
newDeleteBotLocale pBotId_ pBotVersion_ pLocaleId_ =
  DeleteBotLocale'
    { botId = pBotId_,
      botVersion = pBotVersion_,
      localeId = pLocaleId_
    }

-- | The unique identifier of the bot that contains the locale.
deleteBotLocale_botId :: Lens.Lens' DeleteBotLocale Prelude.Text
deleteBotLocale_botId = Lens.lens (\DeleteBotLocale' {botId} -> botId) (\s@DeleteBotLocale' {} a -> s {botId = a} :: DeleteBotLocale)

-- | The version of the bot that contains the locale.
deleteBotLocale_botVersion :: Lens.Lens' DeleteBotLocale Prelude.Text
deleteBotLocale_botVersion = Lens.lens (\DeleteBotLocale' {botVersion} -> botVersion) (\s@DeleteBotLocale' {} a -> s {botVersion = a} :: DeleteBotLocale)

-- | The identifier of the language and locale that will be deleted. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
deleteBotLocale_localeId :: Lens.Lens' DeleteBotLocale Prelude.Text
deleteBotLocale_localeId = Lens.lens (\DeleteBotLocale' {localeId} -> localeId) (\s@DeleteBotLocale' {} a -> s {localeId = a} :: DeleteBotLocale)

instance Core.AWSRequest DeleteBotLocale where
  type
    AWSResponse DeleteBotLocale =
      DeleteBotLocaleResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotLocaleResponse'
            Prelude.<$> (x Core..?> "botLocaleStatus")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBotLocale

instance Prelude.NFData DeleteBotLocale

instance Core.ToHeaders DeleteBotLocale where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteBotLocale where
  toPath DeleteBotLocale' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/"
      ]

instance Core.ToQuery DeleteBotLocale where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotLocaleResponse' smart constructor.
data DeleteBotLocaleResponse = DeleteBotLocaleResponse'
  { -- | The status of deleting the bot locale. The locale first enters the
    -- @Deleting@ status. Once the locale is deleted it no longer appears in
    -- the list of locales for the bot.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | The version of the bot that contained the deleted locale.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contained the deleted locale.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the deleted locale.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotLocaleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botLocaleStatus', 'deleteBotLocaleResponse_botLocaleStatus' - The status of deleting the bot locale. The locale first enters the
-- @Deleting@ status. Once the locale is deleted it no longer appears in
-- the list of locales for the bot.
--
-- 'botVersion', 'deleteBotLocaleResponse_botVersion' - The version of the bot that contained the deleted locale.
--
-- 'botId', 'deleteBotLocaleResponse_botId' - The identifier of the bot that contained the deleted locale.
--
-- 'localeId', 'deleteBotLocaleResponse_localeId' - The language and locale of the deleted locale.
--
-- 'httpStatus', 'deleteBotLocaleResponse_httpStatus' - The response's http status code.
newDeleteBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotLocaleResponse
newDeleteBotLocaleResponse pHttpStatus_ =
  DeleteBotLocaleResponse'
    { botLocaleStatus =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of deleting the bot locale. The locale first enters the
-- @Deleting@ status. Once the locale is deleted it no longer appears in
-- the list of locales for the bot.
deleteBotLocaleResponse_botLocaleStatus :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
deleteBotLocaleResponse_botLocaleStatus = Lens.lens (\DeleteBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@DeleteBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: DeleteBotLocaleResponse)

-- | The version of the bot that contained the deleted locale.
deleteBotLocaleResponse_botVersion :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_botVersion = Lens.lens (\DeleteBotLocaleResponse' {botVersion} -> botVersion) (\s@DeleteBotLocaleResponse' {} a -> s {botVersion = a} :: DeleteBotLocaleResponse)

-- | The identifier of the bot that contained the deleted locale.
deleteBotLocaleResponse_botId :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_botId = Lens.lens (\DeleteBotLocaleResponse' {botId} -> botId) (\s@DeleteBotLocaleResponse' {} a -> s {botId = a} :: DeleteBotLocaleResponse)

-- | The language and locale of the deleted locale.
deleteBotLocaleResponse_localeId :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_localeId = Lens.lens (\DeleteBotLocaleResponse' {localeId} -> localeId) (\s@DeleteBotLocaleResponse' {} a -> s {localeId = a} :: DeleteBotLocaleResponse)

-- | The response's http status code.
deleteBotLocaleResponse_httpStatus :: Lens.Lens' DeleteBotLocaleResponse Prelude.Int
deleteBotLocaleResponse_httpStatus = Lens.lens (\DeleteBotLocaleResponse' {httpStatus} -> httpStatus) (\s@DeleteBotLocaleResponse' {} a -> s {httpStatus = a} :: DeleteBotLocaleResponse)

instance Prelude.NFData DeleteBotLocaleResponse
