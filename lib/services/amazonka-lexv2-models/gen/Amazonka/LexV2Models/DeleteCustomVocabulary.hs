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
-- Module      : Amazonka.LexV2Models.DeleteCustomVocabulary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a custom vocabulary from the specified locale in the specified
-- bot.
module Amazonka.LexV2Models.DeleteCustomVocabulary
  ( -- * Creating a Request
    DeleteCustomVocabulary (..),
    newDeleteCustomVocabulary,

    -- * Request Lenses
    deleteCustomVocabulary_botId,
    deleteCustomVocabulary_botVersion,
    deleteCustomVocabulary_localeId,

    -- * Destructuring the Response
    DeleteCustomVocabularyResponse (..),
    newDeleteCustomVocabularyResponse,

    -- * Response Lenses
    deleteCustomVocabularyResponse_botId,
    deleteCustomVocabularyResponse_botVersion,
    deleteCustomVocabularyResponse_customVocabularyStatus,
    deleteCustomVocabularyResponse_localeId,
    deleteCustomVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomVocabulary' smart constructor.
data DeleteCustomVocabulary = DeleteCustomVocabulary'
  { -- | The unique identifier of the bot to remove the custom vocabulary from.
    botId :: Prelude.Text,
    -- | The version of the bot to remove the custom vocabulary from.
    botVersion :: Prelude.Text,
    -- | The locale identifier for the locale that contains the custom vocabulary
    -- to remove.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteCustomVocabulary_botId' - The unique identifier of the bot to remove the custom vocabulary from.
--
-- 'botVersion', 'deleteCustomVocabulary_botVersion' - The version of the bot to remove the custom vocabulary from.
--
-- 'localeId', 'deleteCustomVocabulary_localeId' - The locale identifier for the locale that contains the custom vocabulary
-- to remove.
newDeleteCustomVocabulary ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DeleteCustomVocabulary
newDeleteCustomVocabulary
  pBotId_
  pBotVersion_
  pLocaleId_ =
    DeleteCustomVocabulary'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The unique identifier of the bot to remove the custom vocabulary from.
deleteCustomVocabulary_botId :: Lens.Lens' DeleteCustomVocabulary Prelude.Text
deleteCustomVocabulary_botId = Lens.lens (\DeleteCustomVocabulary' {botId} -> botId) (\s@DeleteCustomVocabulary' {} a -> s {botId = a} :: DeleteCustomVocabulary)

-- | The version of the bot to remove the custom vocabulary from.
deleteCustomVocabulary_botVersion :: Lens.Lens' DeleteCustomVocabulary Prelude.Text
deleteCustomVocabulary_botVersion = Lens.lens (\DeleteCustomVocabulary' {botVersion} -> botVersion) (\s@DeleteCustomVocabulary' {} a -> s {botVersion = a} :: DeleteCustomVocabulary)

-- | The locale identifier for the locale that contains the custom vocabulary
-- to remove.
deleteCustomVocabulary_localeId :: Lens.Lens' DeleteCustomVocabulary Prelude.Text
deleteCustomVocabulary_localeId = Lens.lens (\DeleteCustomVocabulary' {localeId} -> localeId) (\s@DeleteCustomVocabulary' {} a -> s {localeId = a} :: DeleteCustomVocabulary)

instance Core.AWSRequest DeleteCustomVocabulary where
  type
    AWSResponse DeleteCustomVocabulary =
      DeleteCustomVocabularyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCustomVocabularyResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "customVocabularyStatus")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomVocabulary where
  hashWithSalt _salt DeleteCustomVocabulary' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DeleteCustomVocabulary where
  rnf DeleteCustomVocabulary' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf localeId

instance Data.ToHeaders DeleteCustomVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCustomVocabulary where
  toPath DeleteCustomVocabulary' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary"
      ]

instance Data.ToQuery DeleteCustomVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomVocabularyResponse' smart constructor.
data DeleteCustomVocabularyResponse = DeleteCustomVocabularyResponse'
  { -- | The identifier of the bot that the custom vocabulary was removed from.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that the custom vocabulary was removed from.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of removing the custom vocabulary.
    customVocabularyStatus :: Prelude.Maybe CustomVocabularyStatus,
    -- | The locale identifier for the locale that the custom vocabulary was
    -- removed from.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteCustomVocabularyResponse_botId' - The identifier of the bot that the custom vocabulary was removed from.
--
-- 'botVersion', 'deleteCustomVocabularyResponse_botVersion' - The version of the bot that the custom vocabulary was removed from.
--
-- 'customVocabularyStatus', 'deleteCustomVocabularyResponse_customVocabularyStatus' - The status of removing the custom vocabulary.
--
-- 'localeId', 'deleteCustomVocabularyResponse_localeId' - The locale identifier for the locale that the custom vocabulary was
-- removed from.
--
-- 'httpStatus', 'deleteCustomVocabularyResponse_httpStatus' - The response's http status code.
newDeleteCustomVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomVocabularyResponse
newDeleteCustomVocabularyResponse pHttpStatus_ =
  DeleteCustomVocabularyResponse'
    { botId =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      customVocabularyStatus = Prelude.Nothing,
      localeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that the custom vocabulary was removed from.
deleteCustomVocabularyResponse_botId :: Lens.Lens' DeleteCustomVocabularyResponse (Prelude.Maybe Prelude.Text)
deleteCustomVocabularyResponse_botId = Lens.lens (\DeleteCustomVocabularyResponse' {botId} -> botId) (\s@DeleteCustomVocabularyResponse' {} a -> s {botId = a} :: DeleteCustomVocabularyResponse)

-- | The version of the bot that the custom vocabulary was removed from.
deleteCustomVocabularyResponse_botVersion :: Lens.Lens' DeleteCustomVocabularyResponse (Prelude.Maybe Prelude.Text)
deleteCustomVocabularyResponse_botVersion = Lens.lens (\DeleteCustomVocabularyResponse' {botVersion} -> botVersion) (\s@DeleteCustomVocabularyResponse' {} a -> s {botVersion = a} :: DeleteCustomVocabularyResponse)

-- | The status of removing the custom vocabulary.
deleteCustomVocabularyResponse_customVocabularyStatus :: Lens.Lens' DeleteCustomVocabularyResponse (Prelude.Maybe CustomVocabularyStatus)
deleteCustomVocabularyResponse_customVocabularyStatus = Lens.lens (\DeleteCustomVocabularyResponse' {customVocabularyStatus} -> customVocabularyStatus) (\s@DeleteCustomVocabularyResponse' {} a -> s {customVocabularyStatus = a} :: DeleteCustomVocabularyResponse)

-- | The locale identifier for the locale that the custom vocabulary was
-- removed from.
deleteCustomVocabularyResponse_localeId :: Lens.Lens' DeleteCustomVocabularyResponse (Prelude.Maybe Prelude.Text)
deleteCustomVocabularyResponse_localeId = Lens.lens (\DeleteCustomVocabularyResponse' {localeId} -> localeId) (\s@DeleteCustomVocabularyResponse' {} a -> s {localeId = a} :: DeleteCustomVocabularyResponse)

-- | The response's http status code.
deleteCustomVocabularyResponse_httpStatus :: Lens.Lens' DeleteCustomVocabularyResponse Prelude.Int
deleteCustomVocabularyResponse_httpStatus = Lens.lens (\DeleteCustomVocabularyResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomVocabularyResponse' {} a -> s {httpStatus = a} :: DeleteCustomVocabularyResponse)

instance
  Prelude.NFData
    DeleteCustomVocabularyResponse
  where
  rnf DeleteCustomVocabularyResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf customVocabularyStatus `Prelude.seq`
          Prelude.rnf localeId `Prelude.seq`
            Prelude.rnf httpStatus
