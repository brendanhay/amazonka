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
-- Module      : Network.AWS.LexV2Models.UpdateSlotType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing slot type.
module Network.AWS.LexV2Models.UpdateSlotType
  ( -- * Creating a Request
    UpdateSlotType (..),
    newUpdateSlotType,

    -- * Request Lenses
    updateSlotType_parentSlotTypeSignature,
    updateSlotType_slotTypeValues,
    updateSlotType_description,
    updateSlotType_slotTypeId,
    updateSlotType_slotTypeName,
    updateSlotType_valueSelectionSetting,
    updateSlotType_botId,
    updateSlotType_botVersion,
    updateSlotType_localeId,

    -- * Destructuring the Response
    UpdateSlotTypeResponse (..),
    newUpdateSlotTypeResponse,

    -- * Response Lenses
    updateSlotTypeResponse_parentSlotTypeSignature,
    updateSlotTypeResponse_slotTypeValues,
    updateSlotTypeResponse_valueSelectionSetting,
    updateSlotTypeResponse_botVersion,
    updateSlotTypeResponse_lastUpdatedDateTime,
    updateSlotTypeResponse_botId,
    updateSlotTypeResponse_localeId,
    updateSlotTypeResponse_creationDateTime,
    updateSlotTypeResponse_slotTypeName,
    updateSlotTypeResponse_description,
    updateSlotTypeResponse_slotTypeId,
    updateSlotTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSlotType' smart constructor.
data UpdateSlotType = UpdateSlotType'
  { -- | The new built-in slot type that should be used as the parent of this
    -- slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A new list of values and their optional synonyms that define the values
    -- that the slot type can take.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The new description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the slot type to update.
    slotTypeId :: Prelude.Text,
    -- | The new name of the slot type.
    slotTypeName :: Prelude.Text,
    -- | The strategy that Amazon Lex should use when deciding on a value from
    -- the list of slot type values.
    valueSelectionSetting :: SlotValueSelectionSetting,
    -- | The identifier of the bot that contains the slot type.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the slot type. Must be @DRAFT@.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that contains the slot type.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'updateSlotType_parentSlotTypeSignature' - The new built-in slot type that should be used as the parent of this
-- slot type.
--
-- 'slotTypeValues', 'updateSlotType_slotTypeValues' - A new list of values and their optional synonyms that define the values
-- that the slot type can take.
--
-- 'description', 'updateSlotType_description' - The new description of the slot type.
--
-- 'slotTypeId', 'updateSlotType_slotTypeId' - The unique identifier of the slot type to update.
--
-- 'slotTypeName', 'updateSlotType_slotTypeName' - The new name of the slot type.
--
-- 'valueSelectionSetting', 'updateSlotType_valueSelectionSetting' - The strategy that Amazon Lex should use when deciding on a value from
-- the list of slot type values.
--
-- 'botId', 'updateSlotType_botId' - The identifier of the bot that contains the slot type.
--
-- 'botVersion', 'updateSlotType_botVersion' - The version of the bot that contains the slot type. Must be @DRAFT@.
--
-- 'localeId', 'updateSlotType_localeId' - The identifier of the language and locale that contains the slot type.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newUpdateSlotType ::
  -- | 'slotTypeId'
  Prelude.Text ->
  -- | 'slotTypeName'
  Prelude.Text ->
  -- | 'valueSelectionSetting'
  SlotValueSelectionSetting ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  UpdateSlotType
newUpdateSlotType
  pSlotTypeId_
  pSlotTypeName_
  pValueSelectionSetting_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    UpdateSlotType'
      { parentSlotTypeSignature =
          Prelude.Nothing,
        slotTypeValues = Prelude.Nothing,
        description = Prelude.Nothing,
        slotTypeId = pSlotTypeId_,
        slotTypeName = pSlotTypeName_,
        valueSelectionSetting = pValueSelectionSetting_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The new built-in slot type that should be used as the parent of this
-- slot type.
updateSlotType_parentSlotTypeSignature :: Lens.Lens' UpdateSlotType (Prelude.Maybe Prelude.Text)
updateSlotType_parentSlotTypeSignature = Lens.lens (\UpdateSlotType' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@UpdateSlotType' {} a -> s {parentSlotTypeSignature = a} :: UpdateSlotType)

-- | A new list of values and their optional synonyms that define the values
-- that the slot type can take.
updateSlotType_slotTypeValues :: Lens.Lens' UpdateSlotType (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
updateSlotType_slotTypeValues = Lens.lens (\UpdateSlotType' {slotTypeValues} -> slotTypeValues) (\s@UpdateSlotType' {} a -> s {slotTypeValues = a} :: UpdateSlotType) Prelude.. Lens.mapping Lens.coerced

-- | The new description of the slot type.
updateSlotType_description :: Lens.Lens' UpdateSlotType (Prelude.Maybe Prelude.Text)
updateSlotType_description = Lens.lens (\UpdateSlotType' {description} -> description) (\s@UpdateSlotType' {} a -> s {description = a} :: UpdateSlotType)

-- | The unique identifier of the slot type to update.
updateSlotType_slotTypeId :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_slotTypeId = Lens.lens (\UpdateSlotType' {slotTypeId} -> slotTypeId) (\s@UpdateSlotType' {} a -> s {slotTypeId = a} :: UpdateSlotType)

-- | The new name of the slot type.
updateSlotType_slotTypeName :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_slotTypeName = Lens.lens (\UpdateSlotType' {slotTypeName} -> slotTypeName) (\s@UpdateSlotType' {} a -> s {slotTypeName = a} :: UpdateSlotType)

-- | The strategy that Amazon Lex should use when deciding on a value from
-- the list of slot type values.
updateSlotType_valueSelectionSetting :: Lens.Lens' UpdateSlotType SlotValueSelectionSetting
updateSlotType_valueSelectionSetting = Lens.lens (\UpdateSlotType' {valueSelectionSetting} -> valueSelectionSetting) (\s@UpdateSlotType' {} a -> s {valueSelectionSetting = a} :: UpdateSlotType)

-- | The identifier of the bot that contains the slot type.
updateSlotType_botId :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_botId = Lens.lens (\UpdateSlotType' {botId} -> botId) (\s@UpdateSlotType' {} a -> s {botId = a} :: UpdateSlotType)

-- | The version of the bot that contains the slot type. Must be @DRAFT@.
updateSlotType_botVersion :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_botVersion = Lens.lens (\UpdateSlotType' {botVersion} -> botVersion) (\s@UpdateSlotType' {} a -> s {botVersion = a} :: UpdateSlotType)

-- | The identifier of the language and locale that contains the slot type.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
updateSlotType_localeId :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_localeId = Lens.lens (\UpdateSlotType' {localeId} -> localeId) (\s@UpdateSlotType' {} a -> s {localeId = a} :: UpdateSlotType)

instance Core.AWSRequest UpdateSlotType where
  type
    AWSResponse UpdateSlotType =
      UpdateSlotTypeResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSlotTypeResponse'
            Prelude.<$> (x Core..?> "parentSlotTypeSignature")
            Prelude.<*> (x Core..?> "slotTypeValues")
            Prelude.<*> (x Core..?> "valueSelectionSetting")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "slotTypeName")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "slotTypeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSlotType

instance Prelude.NFData UpdateSlotType

instance Core.ToHeaders UpdateSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSlotType where
  toJSON UpdateSlotType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("parentSlotTypeSignature" Core..=)
              Prelude.<$> parentSlotTypeSignature,
            ("slotTypeValues" Core..=)
              Prelude.<$> slotTypeValues,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("slotTypeName" Core..= slotTypeName),
            Prelude.Just
              ( "valueSelectionSetting"
                  Core..= valueSelectionSetting
              )
          ]
      )

instance Core.ToPath UpdateSlotType where
  toPath UpdateSlotType' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/slottypes/",
        Core.toBS slotTypeId,
        "/"
      ]

instance Core.ToQuery UpdateSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSlotTypeResponse' smart constructor.
data UpdateSlotTypeResponse = UpdateSlotTypeResponse'
  { -- | The updated signature of the built-in slot type that is the parent of
    -- this slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The updated values that the slot type provides.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The updated strategy that Amazon Lex uses to determine which value to
    -- select from the slot type.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The version of the bot that contains the slot type. This is always
    -- @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the bot that contains the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the updated slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The updated name of the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the updated slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'updateSlotTypeResponse_parentSlotTypeSignature' - The updated signature of the built-in slot type that is the parent of
-- this slot type.
--
-- 'slotTypeValues', 'updateSlotTypeResponse_slotTypeValues' - The updated values that the slot type provides.
--
-- 'valueSelectionSetting', 'updateSlotTypeResponse_valueSelectionSetting' - The updated strategy that Amazon Lex uses to determine which value to
-- select from the slot type.
--
-- 'botVersion', 'updateSlotTypeResponse_botVersion' - The version of the bot that contains the slot type. This is always
-- @DRAFT@.
--
-- 'lastUpdatedDateTime', 'updateSlotTypeResponse_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
--
-- 'botId', 'updateSlotTypeResponse_botId' - The identifier of the bot that contains the slot type.
--
-- 'localeId', 'updateSlotTypeResponse_localeId' - The language and locale of the updated slot type.
--
-- 'creationDateTime', 'updateSlotTypeResponse_creationDateTime' - The timestamp of the date and time that the slot type was created.
--
-- 'slotTypeName', 'updateSlotTypeResponse_slotTypeName' - The updated name of the slot type.
--
-- 'description', 'updateSlotTypeResponse_description' - The updated description of the slot type.
--
-- 'slotTypeId', 'updateSlotTypeResponse_slotTypeId' - The unique identifier of the updated slot type.
--
-- 'httpStatus', 'updateSlotTypeResponse_httpStatus' - The response's http status code.
newUpdateSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSlotTypeResponse
newUpdateSlotTypeResponse pHttpStatus_ =
  UpdateSlotTypeResponse'
    { parentSlotTypeSignature =
        Prelude.Nothing,
      slotTypeValues = Prelude.Nothing,
      valueSelectionSetting = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      description = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated signature of the built-in slot type that is the parent of
-- this slot type.
updateSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\UpdateSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@UpdateSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: UpdateSlotTypeResponse)

-- | The updated values that the slot type provides.
updateSlotTypeResponse_slotTypeValues :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
updateSlotTypeResponse_slotTypeValues = Lens.lens (\UpdateSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeValues = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated strategy that Amazon Lex uses to determine which value to
-- select from the slot type.
updateSlotTypeResponse_valueSelectionSetting :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
updateSlotTypeResponse_valueSelectionSetting = Lens.lens (\UpdateSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@UpdateSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: UpdateSlotTypeResponse)

-- | The version of the bot that contains the slot type. This is always
-- @DRAFT@.
updateSlotTypeResponse_botVersion :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_botVersion = Lens.lens (\UpdateSlotTypeResponse' {botVersion} -> botVersion) (\s@UpdateSlotTypeResponse' {} a -> s {botVersion = a} :: UpdateSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was last updated.
updateSlotTypeResponse_lastUpdatedDateTime :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotTypeResponse_lastUpdatedDateTime = Lens.lens (\UpdateSlotTypeResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateSlotTypeResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The identifier of the bot that contains the slot type.
updateSlotTypeResponse_botId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_botId = Lens.lens (\UpdateSlotTypeResponse' {botId} -> botId) (\s@UpdateSlotTypeResponse' {} a -> s {botId = a} :: UpdateSlotTypeResponse)

-- | The language and locale of the updated slot type.
updateSlotTypeResponse_localeId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_localeId = Lens.lens (\UpdateSlotTypeResponse' {localeId} -> localeId) (\s@UpdateSlotTypeResponse' {} a -> s {localeId = a} :: UpdateSlotTypeResponse)

-- | The timestamp of the date and time that the slot type was created.
updateSlotTypeResponse_creationDateTime :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotTypeResponse_creationDateTime = Lens.lens (\UpdateSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@UpdateSlotTypeResponse' {} a -> s {creationDateTime = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The updated name of the slot type.
updateSlotTypeResponse_slotTypeName :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_slotTypeName = Lens.lens (\UpdateSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeName = a} :: UpdateSlotTypeResponse)

-- | The updated description of the slot type.
updateSlotTypeResponse_description :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_description = Lens.lens (\UpdateSlotTypeResponse' {description} -> description) (\s@UpdateSlotTypeResponse' {} a -> s {description = a} :: UpdateSlotTypeResponse)

-- | The unique identifier of the updated slot type.
updateSlotTypeResponse_slotTypeId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_slotTypeId = Lens.lens (\UpdateSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeId = a} :: UpdateSlotTypeResponse)

-- | The response's http status code.
updateSlotTypeResponse_httpStatus :: Lens.Lens' UpdateSlotTypeResponse Prelude.Int
updateSlotTypeResponse_httpStatus = Lens.lens (\UpdateSlotTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateSlotTypeResponse' {} a -> s {httpStatus = a} :: UpdateSlotTypeResponse)

instance Prelude.NFData UpdateSlotTypeResponse
