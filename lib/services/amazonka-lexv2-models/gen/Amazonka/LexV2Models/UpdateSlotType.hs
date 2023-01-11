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
-- Module      : Amazonka.LexV2Models.UpdateSlotType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing slot type.
module Amazonka.LexV2Models.UpdateSlotType
  ( -- * Creating a Request
    UpdateSlotType (..),
    newUpdateSlotType,

    -- * Request Lenses
    updateSlotType_compositeSlotTypeSetting,
    updateSlotType_description,
    updateSlotType_externalSourceSetting,
    updateSlotType_parentSlotTypeSignature,
    updateSlotType_slotTypeValues,
    updateSlotType_valueSelectionSetting,
    updateSlotType_slotTypeId,
    updateSlotType_slotTypeName,
    updateSlotType_botId,
    updateSlotType_botVersion,
    updateSlotType_localeId,

    -- * Destructuring the Response
    UpdateSlotTypeResponse (..),
    newUpdateSlotTypeResponse,

    -- * Response Lenses
    updateSlotTypeResponse_botId,
    updateSlotTypeResponse_botVersion,
    updateSlotTypeResponse_compositeSlotTypeSetting,
    updateSlotTypeResponse_creationDateTime,
    updateSlotTypeResponse_description,
    updateSlotTypeResponse_externalSourceSetting,
    updateSlotTypeResponse_lastUpdatedDateTime,
    updateSlotTypeResponse_localeId,
    updateSlotTypeResponse_parentSlotTypeSignature,
    updateSlotTypeResponse_slotTypeId,
    updateSlotTypeResponse_slotTypeName,
    updateSlotTypeResponse_slotTypeValues,
    updateSlotTypeResponse_valueSelectionSetting,
    updateSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSlotType' smart constructor.
data UpdateSlotType = UpdateSlotType'
  { -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | The new description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | The new built-in slot type that should be used as the parent of this
    -- slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A new list of values and their optional synonyms that define the values
    -- that the slot type can take.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The strategy that Amazon Lex should use when deciding on a value from
    -- the list of slot type values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The unique identifier of the slot type to update.
    slotTypeId :: Prelude.Text,
    -- | The new name of the slot type.
    slotTypeName :: Prelude.Text,
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
-- 'compositeSlotTypeSetting', 'updateSlotType_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'description', 'updateSlotType_description' - The new description of the slot type.
--
-- 'externalSourceSetting', 'updateSlotType_externalSourceSetting' - Undocumented member.
--
-- 'parentSlotTypeSignature', 'updateSlotType_parentSlotTypeSignature' - The new built-in slot type that should be used as the parent of this
-- slot type.
--
-- 'slotTypeValues', 'updateSlotType_slotTypeValues' - A new list of values and their optional synonyms that define the values
-- that the slot type can take.
--
-- 'valueSelectionSetting', 'updateSlotType_valueSelectionSetting' - The strategy that Amazon Lex should use when deciding on a value from
-- the list of slot type values.
--
-- 'slotTypeId', 'updateSlotType_slotTypeId' - The unique identifier of the slot type to update.
--
-- 'slotTypeName', 'updateSlotType_slotTypeName' - The new name of the slot type.
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
  pBotId_
  pBotVersion_
  pLocaleId_ =
    UpdateSlotType'
      { compositeSlotTypeSetting =
          Prelude.Nothing,
        description = Prelude.Nothing,
        externalSourceSetting = Prelude.Nothing,
        parentSlotTypeSignature = Prelude.Nothing,
        slotTypeValues = Prelude.Nothing,
        valueSelectionSetting = Prelude.Nothing,
        slotTypeId = pSlotTypeId_,
        slotTypeName = pSlotTypeName_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | Specifications for a composite slot type.
updateSlotType_compositeSlotTypeSetting :: Lens.Lens' UpdateSlotType (Prelude.Maybe CompositeSlotTypeSetting)
updateSlotType_compositeSlotTypeSetting = Lens.lens (\UpdateSlotType' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@UpdateSlotType' {} a -> s {compositeSlotTypeSetting = a} :: UpdateSlotType)

-- | The new description of the slot type.
updateSlotType_description :: Lens.Lens' UpdateSlotType (Prelude.Maybe Prelude.Text)
updateSlotType_description = Lens.lens (\UpdateSlotType' {description} -> description) (\s@UpdateSlotType' {} a -> s {description = a} :: UpdateSlotType)

-- | Undocumented member.
updateSlotType_externalSourceSetting :: Lens.Lens' UpdateSlotType (Prelude.Maybe ExternalSourceSetting)
updateSlotType_externalSourceSetting = Lens.lens (\UpdateSlotType' {externalSourceSetting} -> externalSourceSetting) (\s@UpdateSlotType' {} a -> s {externalSourceSetting = a} :: UpdateSlotType)

-- | The new built-in slot type that should be used as the parent of this
-- slot type.
updateSlotType_parentSlotTypeSignature :: Lens.Lens' UpdateSlotType (Prelude.Maybe Prelude.Text)
updateSlotType_parentSlotTypeSignature = Lens.lens (\UpdateSlotType' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@UpdateSlotType' {} a -> s {parentSlotTypeSignature = a} :: UpdateSlotType)

-- | A new list of values and their optional synonyms that define the values
-- that the slot type can take.
updateSlotType_slotTypeValues :: Lens.Lens' UpdateSlotType (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
updateSlotType_slotTypeValues = Lens.lens (\UpdateSlotType' {slotTypeValues} -> slotTypeValues) (\s@UpdateSlotType' {} a -> s {slotTypeValues = a} :: UpdateSlotType) Prelude.. Lens.mapping Lens.coerced

-- | The strategy that Amazon Lex should use when deciding on a value from
-- the list of slot type values.
updateSlotType_valueSelectionSetting :: Lens.Lens' UpdateSlotType (Prelude.Maybe SlotValueSelectionSetting)
updateSlotType_valueSelectionSetting = Lens.lens (\UpdateSlotType' {valueSelectionSetting} -> valueSelectionSetting) (\s@UpdateSlotType' {} a -> s {valueSelectionSetting = a} :: UpdateSlotType)

-- | The unique identifier of the slot type to update.
updateSlotType_slotTypeId :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_slotTypeId = Lens.lens (\UpdateSlotType' {slotTypeId} -> slotTypeId) (\s@UpdateSlotType' {} a -> s {slotTypeId = a} :: UpdateSlotType)

-- | The new name of the slot type.
updateSlotType_slotTypeName :: Lens.Lens' UpdateSlotType Prelude.Text
updateSlotType_slotTypeName = Lens.lens (\UpdateSlotType' {slotTypeName} -> slotTypeName) (\s@UpdateSlotType' {} a -> s {slotTypeName = a} :: UpdateSlotType)

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSlotTypeResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "compositeSlotTypeSetting")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "externalSourceSetting")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "parentSlotTypeSignature")
            Prelude.<*> (x Data..?> "slotTypeId")
            Prelude.<*> (x Data..?> "slotTypeName")
            Prelude.<*> (x Data..?> "slotTypeValues")
            Prelude.<*> (x Data..?> "valueSelectionSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSlotType where
  hashWithSalt _salt UpdateSlotType' {..} =
    _salt
      `Prelude.hashWithSalt` compositeSlotTypeSetting
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` externalSourceSetting
      `Prelude.hashWithSalt` parentSlotTypeSignature
      `Prelude.hashWithSalt` slotTypeValues
      `Prelude.hashWithSalt` valueSelectionSetting
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` slotTypeName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData UpdateSlotType where
  rnf UpdateSlotType' {..} =
    Prelude.rnf compositeSlotTypeSetting
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf externalSourceSetting
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf slotTypeValues
      `Prelude.seq` Prelude.rnf valueSelectionSetting
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf slotTypeName
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders UpdateSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSlotType where
  toJSON UpdateSlotType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("compositeSlotTypeSetting" Data..=)
              Prelude.<$> compositeSlotTypeSetting,
            ("description" Data..=) Prelude.<$> description,
            ("externalSourceSetting" Data..=)
              Prelude.<$> externalSourceSetting,
            ("parentSlotTypeSignature" Data..=)
              Prelude.<$> parentSlotTypeSignature,
            ("slotTypeValues" Data..=)
              Prelude.<$> slotTypeValues,
            ("valueSelectionSetting" Data..=)
              Prelude.<$> valueSelectionSetting,
            Prelude.Just ("slotTypeName" Data..= slotTypeName)
          ]
      )

instance Data.ToPath UpdateSlotType where
  toPath UpdateSlotType' {..} =
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

instance Data.ToQuery UpdateSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSlotTypeResponse' smart constructor.
data UpdateSlotTypeResponse = UpdateSlotTypeResponse'
  { -- | The identifier of the bot that contains the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that contains the slot type. This is always
    -- @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | The timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The updated description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The language and locale of the updated slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The updated signature of the built-in slot type that is the parent of
    -- this slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the updated slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The updated name of the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The updated values that the slot type provides.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The updated strategy that Amazon Lex uses to determine which value to
    -- select from the slot type.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
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
-- 'botId', 'updateSlotTypeResponse_botId' - The identifier of the bot that contains the slot type.
--
-- 'botVersion', 'updateSlotTypeResponse_botVersion' - The version of the bot that contains the slot type. This is always
-- @DRAFT@.
--
-- 'compositeSlotTypeSetting', 'updateSlotTypeResponse_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'creationDateTime', 'updateSlotTypeResponse_creationDateTime' - The timestamp of the date and time that the slot type was created.
--
-- 'description', 'updateSlotTypeResponse_description' - The updated description of the slot type.
--
-- 'externalSourceSetting', 'updateSlotTypeResponse_externalSourceSetting' - Undocumented member.
--
-- 'lastUpdatedDateTime', 'updateSlotTypeResponse_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
--
-- 'localeId', 'updateSlotTypeResponse_localeId' - The language and locale of the updated slot type.
--
-- 'parentSlotTypeSignature', 'updateSlotTypeResponse_parentSlotTypeSignature' - The updated signature of the built-in slot type that is the parent of
-- this slot type.
--
-- 'slotTypeId', 'updateSlotTypeResponse_slotTypeId' - The unique identifier of the updated slot type.
--
-- 'slotTypeName', 'updateSlotTypeResponse_slotTypeName' - The updated name of the slot type.
--
-- 'slotTypeValues', 'updateSlotTypeResponse_slotTypeValues' - The updated values that the slot type provides.
--
-- 'valueSelectionSetting', 'updateSlotTypeResponse_valueSelectionSetting' - The updated strategy that Amazon Lex uses to determine which value to
-- select from the slot type.
--
-- 'httpStatus', 'updateSlotTypeResponse_httpStatus' - The response's http status code.
newUpdateSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSlotTypeResponse
newUpdateSlotTypeResponse pHttpStatus_ =
  UpdateSlotTypeResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      compositeSlotTypeSetting = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      externalSourceSetting = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      localeId = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      slotTypeValues = Prelude.Nothing,
      valueSelectionSetting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that contains the slot type.
updateSlotTypeResponse_botId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_botId = Lens.lens (\UpdateSlotTypeResponse' {botId} -> botId) (\s@UpdateSlotTypeResponse' {} a -> s {botId = a} :: UpdateSlotTypeResponse)

-- | The version of the bot that contains the slot type. This is always
-- @DRAFT@.
updateSlotTypeResponse_botVersion :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_botVersion = Lens.lens (\UpdateSlotTypeResponse' {botVersion} -> botVersion) (\s@UpdateSlotTypeResponse' {} a -> s {botVersion = a} :: UpdateSlotTypeResponse)

-- | Specifications for a composite slot type.
updateSlotTypeResponse_compositeSlotTypeSetting :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe CompositeSlotTypeSetting)
updateSlotTypeResponse_compositeSlotTypeSetting = Lens.lens (\UpdateSlotTypeResponse' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@UpdateSlotTypeResponse' {} a -> s {compositeSlotTypeSetting = a} :: UpdateSlotTypeResponse)

-- | The timestamp of the date and time that the slot type was created.
updateSlotTypeResponse_creationDateTime :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotTypeResponse_creationDateTime = Lens.lens (\UpdateSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@UpdateSlotTypeResponse' {} a -> s {creationDateTime = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The updated description of the slot type.
updateSlotTypeResponse_description :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_description = Lens.lens (\UpdateSlotTypeResponse' {description} -> description) (\s@UpdateSlotTypeResponse' {} a -> s {description = a} :: UpdateSlotTypeResponse)

-- | Undocumented member.
updateSlotTypeResponse_externalSourceSetting :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe ExternalSourceSetting)
updateSlotTypeResponse_externalSourceSetting = Lens.lens (\UpdateSlotTypeResponse' {externalSourceSetting} -> externalSourceSetting) (\s@UpdateSlotTypeResponse' {} a -> s {externalSourceSetting = a} :: UpdateSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was last updated.
updateSlotTypeResponse_lastUpdatedDateTime :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotTypeResponse_lastUpdatedDateTime = Lens.lens (\UpdateSlotTypeResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateSlotTypeResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The language and locale of the updated slot type.
updateSlotTypeResponse_localeId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_localeId = Lens.lens (\UpdateSlotTypeResponse' {localeId} -> localeId) (\s@UpdateSlotTypeResponse' {} a -> s {localeId = a} :: UpdateSlotTypeResponse)

-- | The updated signature of the built-in slot type that is the parent of
-- this slot type.
updateSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\UpdateSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@UpdateSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: UpdateSlotTypeResponse)

-- | The unique identifier of the updated slot type.
updateSlotTypeResponse_slotTypeId :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_slotTypeId = Lens.lens (\UpdateSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeId = a} :: UpdateSlotTypeResponse)

-- | The updated name of the slot type.
updateSlotTypeResponse_slotTypeName :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe Prelude.Text)
updateSlotTypeResponse_slotTypeName = Lens.lens (\UpdateSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeName = a} :: UpdateSlotTypeResponse)

-- | The updated values that the slot type provides.
updateSlotTypeResponse_slotTypeValues :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
updateSlotTypeResponse_slotTypeValues = Lens.lens (\UpdateSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@UpdateSlotTypeResponse' {} a -> s {slotTypeValues = a} :: UpdateSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated strategy that Amazon Lex uses to determine which value to
-- select from the slot type.
updateSlotTypeResponse_valueSelectionSetting :: Lens.Lens' UpdateSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
updateSlotTypeResponse_valueSelectionSetting = Lens.lens (\UpdateSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@UpdateSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: UpdateSlotTypeResponse)

-- | The response's http status code.
updateSlotTypeResponse_httpStatus :: Lens.Lens' UpdateSlotTypeResponse Prelude.Int
updateSlotTypeResponse_httpStatus = Lens.lens (\UpdateSlotTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateSlotTypeResponse' {} a -> s {httpStatus = a} :: UpdateSlotTypeResponse)

instance Prelude.NFData UpdateSlotTypeResponse where
  rnf UpdateSlotTypeResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf compositeSlotTypeSetting
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf externalSourceSetting
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf slotTypeName
      `Prelude.seq` Prelude.rnf slotTypeValues
      `Prelude.seq` Prelude.rnf valueSelectionSetting
      `Prelude.seq` Prelude.rnf httpStatus
