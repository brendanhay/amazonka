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
-- Module      : Amazonka.LexV2Models.DeleteIntent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified intent.
--
-- Deleting an intent also deletes the slots associated with the intent.
module Amazonka.LexV2Models.DeleteIntent
  ( -- * Creating a Request
    DeleteIntent (..),
    newDeleteIntent,

    -- * Request Lenses
    deleteIntent_intentId,
    deleteIntent_botId,
    deleteIntent_botVersion,
    deleteIntent_localeId,

    -- * Destructuring the Response
    DeleteIntentResponse (..),
    newDeleteIntentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIntent' smart constructor.
data DeleteIntent = DeleteIntent'
  { -- | The unique identifier of the intent to delete.
    intentId :: Prelude.Text,
    -- | The identifier of the bot associated with the intent.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the intent.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale where the bot will be deleted.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentId', 'deleteIntent_intentId' - The unique identifier of the intent to delete.
--
-- 'botId', 'deleteIntent_botId' - The identifier of the bot associated with the intent.
--
-- 'botVersion', 'deleteIntent_botVersion' - The version of the bot associated with the intent.
--
-- 'localeId', 'deleteIntent_localeId' - The identifier of the language and locale where the bot will be deleted.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newDeleteIntent ::
  -- | 'intentId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DeleteIntent
newDeleteIntent
  pIntentId_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    DeleteIntent'
      { intentId = pIntentId_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The unique identifier of the intent to delete.
deleteIntent_intentId :: Lens.Lens' DeleteIntent Prelude.Text
deleteIntent_intentId = Lens.lens (\DeleteIntent' {intentId} -> intentId) (\s@DeleteIntent' {} a -> s {intentId = a} :: DeleteIntent)

-- | The identifier of the bot associated with the intent.
deleteIntent_botId :: Lens.Lens' DeleteIntent Prelude.Text
deleteIntent_botId = Lens.lens (\DeleteIntent' {botId} -> botId) (\s@DeleteIntent' {} a -> s {botId = a} :: DeleteIntent)

-- | The version of the bot associated with the intent.
deleteIntent_botVersion :: Lens.Lens' DeleteIntent Prelude.Text
deleteIntent_botVersion = Lens.lens (\DeleteIntent' {botVersion} -> botVersion) (\s@DeleteIntent' {} a -> s {botVersion = a} :: DeleteIntent)

-- | The identifier of the language and locale where the bot will be deleted.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
deleteIntent_localeId :: Lens.Lens' DeleteIntent Prelude.Text
deleteIntent_localeId = Lens.lens (\DeleteIntent' {localeId} -> localeId) (\s@DeleteIntent' {} a -> s {localeId = a} :: DeleteIntent)

instance Core.AWSRequest DeleteIntent where
  type AWSResponse DeleteIntent = DeleteIntentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteIntentResponse'

instance Prelude.Hashable DeleteIntent where
  hashWithSalt _salt DeleteIntent' {..} =
    _salt `Prelude.hashWithSalt` intentId
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DeleteIntent where
  rnf DeleteIntent' {..} =
    Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders DeleteIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIntent where
  toPath DeleteIntent' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/intents/",
        Data.toBS intentId,
        "/"
      ]

instance Data.ToQuery DeleteIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntentResponse' smart constructor.
data DeleteIntentResponse = DeleteIntentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentResponse ::
  DeleteIntentResponse
newDeleteIntentResponse = DeleteIntentResponse'

instance Prelude.NFData DeleteIntentResponse where
  rnf _ = ()
