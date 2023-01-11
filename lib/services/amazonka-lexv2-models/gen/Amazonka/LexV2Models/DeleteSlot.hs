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
-- Module      : Amazonka.LexV2Models.DeleteSlot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified slot from an intent.
module Amazonka.LexV2Models.DeleteSlot
  ( -- * Creating a Request
    DeleteSlot (..),
    newDeleteSlot,

    -- * Request Lenses
    deleteSlot_slotId,
    deleteSlot_botId,
    deleteSlot_botVersion,
    deleteSlot_localeId,
    deleteSlot_intentId,

    -- * Destructuring the Response
    DeleteSlotResponse (..),
    newDeleteSlotResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSlot' smart constructor.
data DeleteSlot = DeleteSlot'
  { -- | The identifier of the slot to delete.
    slotId :: Prelude.Text,
    -- | The identifier of the bot associated with the slot to delete.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the slot to delete.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the slot will be deleted
    -- from. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The identifier of the intent associated with the slot.
    intentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotId', 'deleteSlot_slotId' - The identifier of the slot to delete.
--
-- 'botId', 'deleteSlot_botId' - The identifier of the bot associated with the slot to delete.
--
-- 'botVersion', 'deleteSlot_botVersion' - The version of the bot associated with the slot to delete.
--
-- 'localeId', 'deleteSlot_localeId' - The identifier of the language and locale that the slot will be deleted
-- from. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'intentId', 'deleteSlot_intentId' - The identifier of the intent associated with the slot.
newDeleteSlot ::
  -- | 'slotId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'intentId'
  Prelude.Text ->
  DeleteSlot
newDeleteSlot
  pSlotId_
  pBotId_
  pBotVersion_
  pLocaleId_
  pIntentId_ =
    DeleteSlot'
      { slotId = pSlotId_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | The identifier of the slot to delete.
deleteSlot_slotId :: Lens.Lens' DeleteSlot Prelude.Text
deleteSlot_slotId = Lens.lens (\DeleteSlot' {slotId} -> slotId) (\s@DeleteSlot' {} a -> s {slotId = a} :: DeleteSlot)

-- | The identifier of the bot associated with the slot to delete.
deleteSlot_botId :: Lens.Lens' DeleteSlot Prelude.Text
deleteSlot_botId = Lens.lens (\DeleteSlot' {botId} -> botId) (\s@DeleteSlot' {} a -> s {botId = a} :: DeleteSlot)

-- | The version of the bot associated with the slot to delete.
deleteSlot_botVersion :: Lens.Lens' DeleteSlot Prelude.Text
deleteSlot_botVersion = Lens.lens (\DeleteSlot' {botVersion} -> botVersion) (\s@DeleteSlot' {} a -> s {botVersion = a} :: DeleteSlot)

-- | The identifier of the language and locale that the slot will be deleted
-- from. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
deleteSlot_localeId :: Lens.Lens' DeleteSlot Prelude.Text
deleteSlot_localeId = Lens.lens (\DeleteSlot' {localeId} -> localeId) (\s@DeleteSlot' {} a -> s {localeId = a} :: DeleteSlot)

-- | The identifier of the intent associated with the slot.
deleteSlot_intentId :: Lens.Lens' DeleteSlot Prelude.Text
deleteSlot_intentId = Lens.lens (\DeleteSlot' {intentId} -> intentId) (\s@DeleteSlot' {} a -> s {intentId = a} :: DeleteSlot)

instance Core.AWSRequest DeleteSlot where
  type AWSResponse DeleteSlot = DeleteSlotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteSlotResponse'

instance Prelude.Hashable DeleteSlot where
  hashWithSalt _salt DeleteSlot' {..} =
    _salt `Prelude.hashWithSalt` slotId
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` intentId

instance Prelude.NFData DeleteSlot where
  rnf DeleteSlot' {..} =
    Prelude.rnf slotId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf intentId

instance Data.ToHeaders DeleteSlot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSlot where
  toPath DeleteSlot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/intents/",
        Data.toBS intentId,
        "/slots/",
        Data.toBS slotId,
        "/"
      ]

instance Data.ToQuery DeleteSlot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlotResponse' smart constructor.
data DeleteSlotResponse = DeleteSlotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSlotResponse ::
  DeleteSlotResponse
newDeleteSlotResponse = DeleteSlotResponse'

instance Prelude.NFData DeleteSlotResponse where
  rnf _ = ()
