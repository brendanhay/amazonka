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
-- Module      : Amazonka.LexV2Models.DeleteSlotType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a slot type from a bot locale.
--
-- If a slot is using the slot type, Amazon Lex throws a
-- @ResourceInUseException@ exception. To avoid the exception, set the
-- @skipResourceInUseCheck@ parameter to @true@.
module Amazonka.LexV2Models.DeleteSlotType
  ( -- * Creating a Request
    DeleteSlotType (..),
    newDeleteSlotType,

    -- * Request Lenses
    deleteSlotType_skipResourceInUseCheck,
    deleteSlotType_slotTypeId,
    deleteSlotType_botId,
    deleteSlotType_botVersion,
    deleteSlotType_localeId,

    -- * Destructuring the Response
    DeleteSlotTypeResponse (..),
    newDeleteSlotTypeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSlotType' smart constructor.
data DeleteSlotType = DeleteSlotType'
  { -- | By default, the @DeleteSlotType@ operations throws a
    -- @ResourceInUseException@ exception if you try to delete a slot type used
    -- by a slot. Set the @skipResourceInUseCheck@ parameter to @true@ to skip
    -- this check and remove the slot type even if a slot uses it.
    skipResourceInUseCheck :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the slot type to delete.
    slotTypeId :: Prelude.Text,
    -- | The identifier of the bot associated with the slot type.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the slot type will be
    -- deleted from. The string must match one of the supported locales. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipResourceInUseCheck', 'deleteSlotType_skipResourceInUseCheck' - By default, the @DeleteSlotType@ operations throws a
-- @ResourceInUseException@ exception if you try to delete a slot type used
-- by a slot. Set the @skipResourceInUseCheck@ parameter to @true@ to skip
-- this check and remove the slot type even if a slot uses it.
--
-- 'slotTypeId', 'deleteSlotType_slotTypeId' - The identifier of the slot type to delete.
--
-- 'botId', 'deleteSlotType_botId' - The identifier of the bot associated with the slot type.
--
-- 'botVersion', 'deleteSlotType_botVersion' - The version of the bot associated with the slot type.
--
-- 'localeId', 'deleteSlotType_localeId' - The identifier of the language and locale that the slot type will be
-- deleted from. The string must match one of the supported locales. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newDeleteSlotType ::
  -- | 'slotTypeId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DeleteSlotType
newDeleteSlotType
  pSlotTypeId_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    DeleteSlotType'
      { skipResourceInUseCheck =
          Prelude.Nothing,
        slotTypeId = pSlotTypeId_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | By default, the @DeleteSlotType@ operations throws a
-- @ResourceInUseException@ exception if you try to delete a slot type used
-- by a slot. Set the @skipResourceInUseCheck@ parameter to @true@ to skip
-- this check and remove the slot type even if a slot uses it.
deleteSlotType_skipResourceInUseCheck :: Lens.Lens' DeleteSlotType (Prelude.Maybe Prelude.Bool)
deleteSlotType_skipResourceInUseCheck = Lens.lens (\DeleteSlotType' {skipResourceInUseCheck} -> skipResourceInUseCheck) (\s@DeleteSlotType' {} a -> s {skipResourceInUseCheck = a} :: DeleteSlotType)

-- | The identifier of the slot type to delete.
deleteSlotType_slotTypeId :: Lens.Lens' DeleteSlotType Prelude.Text
deleteSlotType_slotTypeId = Lens.lens (\DeleteSlotType' {slotTypeId} -> slotTypeId) (\s@DeleteSlotType' {} a -> s {slotTypeId = a} :: DeleteSlotType)

-- | The identifier of the bot associated with the slot type.
deleteSlotType_botId :: Lens.Lens' DeleteSlotType Prelude.Text
deleteSlotType_botId = Lens.lens (\DeleteSlotType' {botId} -> botId) (\s@DeleteSlotType' {} a -> s {botId = a} :: DeleteSlotType)

-- | The version of the bot associated with the slot type.
deleteSlotType_botVersion :: Lens.Lens' DeleteSlotType Prelude.Text
deleteSlotType_botVersion = Lens.lens (\DeleteSlotType' {botVersion} -> botVersion) (\s@DeleteSlotType' {} a -> s {botVersion = a} :: DeleteSlotType)

-- | The identifier of the language and locale that the slot type will be
-- deleted from. The string must match one of the supported locales. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
deleteSlotType_localeId :: Lens.Lens' DeleteSlotType Prelude.Text
deleteSlotType_localeId = Lens.lens (\DeleteSlotType' {localeId} -> localeId) (\s@DeleteSlotType' {} a -> s {localeId = a} :: DeleteSlotType)

instance Core.AWSRequest DeleteSlotType where
  type
    AWSResponse DeleteSlotType =
      DeleteSlotTypeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteSlotTypeResponse'

instance Prelude.Hashable DeleteSlotType where
  hashWithSalt _salt DeleteSlotType' {..} =
    _salt `Prelude.hashWithSalt` skipResourceInUseCheck
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DeleteSlotType where
  rnf DeleteSlotType' {..} =
    Prelude.rnf skipResourceInUseCheck
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders DeleteSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSlotType where
  toPath DeleteSlotType' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/slottypes/",
        Data.toBS slotTypeId,
        "/"
      ]

instance Data.ToQuery DeleteSlotType where
  toQuery DeleteSlotType' {..} =
    Prelude.mconcat
      [ "skipResourceInUseCheck"
          Data.=: skipResourceInUseCheck
      ]

-- | /See:/ 'newDeleteSlotTypeResponse' smart constructor.
data DeleteSlotTypeResponse = DeleteSlotTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSlotTypeResponse ::
  DeleteSlotTypeResponse
newDeleteSlotTypeResponse = DeleteSlotTypeResponse'

instance Prelude.NFData DeleteSlotTypeResponse where
  rnf _ = ()
