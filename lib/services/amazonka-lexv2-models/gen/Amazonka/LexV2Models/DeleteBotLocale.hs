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
-- Module      : Amazonka.LexV2Models.DeleteBotLocale
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a locale from a bot.
--
-- When you delete a locale, all intents, slots, and slot types defined for
-- the locale are also deleted.
module Amazonka.LexV2Models.DeleteBotLocale
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
    deleteBotLocaleResponse_botVersion,
    deleteBotLocaleResponse_localeId,
    deleteBotLocaleResponse_botId,
    deleteBotLocaleResponse_botLocaleStatus,
    deleteBotLocaleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotLocaleResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botLocaleStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBotLocale where
  hashWithSalt _salt DeleteBotLocale' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DeleteBotLocale where
  rnf DeleteBotLocale' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

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
  { -- | The version of the bot that contained the deleted locale.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the deleted locale.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contained the deleted locale.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The status of deleting the bot locale. The locale first enters the
    -- @Deleting@ status. Once the locale is deleted it no longer appears in
    -- the list of locales for the bot.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
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
-- 'botVersion', 'deleteBotLocaleResponse_botVersion' - The version of the bot that contained the deleted locale.
--
-- 'localeId', 'deleteBotLocaleResponse_localeId' - The language and locale of the deleted locale.
--
-- 'botId', 'deleteBotLocaleResponse_botId' - The identifier of the bot that contained the deleted locale.
--
-- 'botLocaleStatus', 'deleteBotLocaleResponse_botLocaleStatus' - The status of deleting the bot locale. The locale first enters the
-- @Deleting@ status. Once the locale is deleted it no longer appears in
-- the list of locales for the bot.
--
-- 'httpStatus', 'deleteBotLocaleResponse_httpStatus' - The response's http status code.
newDeleteBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotLocaleResponse
newDeleteBotLocaleResponse pHttpStatus_ =
  DeleteBotLocaleResponse'
    { botVersion =
        Prelude.Nothing,
      localeId = Prelude.Nothing,
      botId = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that contained the deleted locale.
deleteBotLocaleResponse_botVersion :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_botVersion = Lens.lens (\DeleteBotLocaleResponse' {botVersion} -> botVersion) (\s@DeleteBotLocaleResponse' {} a -> s {botVersion = a} :: DeleteBotLocaleResponse)

-- | The language and locale of the deleted locale.
deleteBotLocaleResponse_localeId :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_localeId = Lens.lens (\DeleteBotLocaleResponse' {localeId} -> localeId) (\s@DeleteBotLocaleResponse' {} a -> s {localeId = a} :: DeleteBotLocaleResponse)

-- | The identifier of the bot that contained the deleted locale.
deleteBotLocaleResponse_botId :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe Prelude.Text)
deleteBotLocaleResponse_botId = Lens.lens (\DeleteBotLocaleResponse' {botId} -> botId) (\s@DeleteBotLocaleResponse' {} a -> s {botId = a} :: DeleteBotLocaleResponse)

-- | The status of deleting the bot locale. The locale first enters the
-- @Deleting@ status. Once the locale is deleted it no longer appears in
-- the list of locales for the bot.
deleteBotLocaleResponse_botLocaleStatus :: Lens.Lens' DeleteBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
deleteBotLocaleResponse_botLocaleStatus = Lens.lens (\DeleteBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@DeleteBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: DeleteBotLocaleResponse)

-- | The response's http status code.
deleteBotLocaleResponse_httpStatus :: Lens.Lens' DeleteBotLocaleResponse Prelude.Int
deleteBotLocaleResponse_httpStatus = Lens.lens (\DeleteBotLocaleResponse' {httpStatus} -> httpStatus) (\s@DeleteBotLocaleResponse' {} a -> s {httpStatus = a} :: DeleteBotLocaleResponse)

instance Prelude.NFData DeleteBotLocaleResponse where
  rnf DeleteBotLocaleResponse' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botLocaleStatus
      `Prelude.seq` Prelude.rnf httpStatus
