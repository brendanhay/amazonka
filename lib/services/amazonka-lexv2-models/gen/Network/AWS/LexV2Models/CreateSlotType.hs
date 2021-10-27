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
-- Module      : Network.AWS.LexV2Models.CreateSlotType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom slot type
--
-- To create a custom slot type, specify a name for the slot type and a set
-- of enumeration values, the values that a slot of this type can assume.
module Network.AWS.LexV2Models.CreateSlotType
  ( -- * Creating a Request
    CreateSlotType (..),
    newCreateSlotType,

    -- * Request Lenses
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeValues,
    createSlotType_description,
    createSlotType_slotTypeName,
    createSlotType_valueSelectionSetting,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,

    -- * Destructuring the Response
    CreateSlotTypeResponse (..),
    newCreateSlotTypeResponse,

    -- * Response Lenses
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_botId,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_description,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSlotType' smart constructor.
data CreateSlotType = CreateSlotType'
  { -- | The built-in slot type used as a parent of this slot type. When you
    -- define a parent slot type, the new slot type has the configuration of
    -- the parent slot type.
    --
    -- Only @AMAZON.AlphaNumeric@ is supported.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A list of @SlotTypeValue@ objects that defines the values that the slot
    -- type can take. Each value can have a list of synonyms, additional values
    -- that help train the machine learning model about the values that it
    -- resolves for a slot.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | A description of the slot type. Use the description to help identify the
    -- slot type in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name for the slot. A slot type name must be unique within the
    -- account.
    slotTypeName :: Prelude.Text,
    -- | Determines the strategy that Amazon Lex uses to select a value from the
    -- list of possible values. The field can be set to one of the following
    -- values:
    --
    -- -   @OriginalValue@ - Returns the value entered by the user, if the user
    --     value is similar to the slot value.
    --
    -- -   @TopResolution@ - If there is a resolution list for the slot, return
    --     the first value in the resolution list. If there is no resolution
    --     list, return null.
    --
    -- If you don\'t specify the @valueSelectionSetting@ parameter, the default
    -- is @OriginalValue@.
    valueSelectionSetting :: SlotValueSelectionSetting,
    -- | The identifier of the bot associated with this slot type.
    botId :: Prelude.Text,
    -- | The identifier of the bot version associated with this slot type.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the slot type will be
    -- used in. The string must match one of the supported locales. All of the
    -- bots, intents, and slots used by the slot type must have the same
    -- locale. For more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'createSlotType_parentSlotTypeSignature' - The built-in slot type used as a parent of this slot type. When you
-- define a parent slot type, the new slot type has the configuration of
-- the parent slot type.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
--
-- 'slotTypeValues', 'createSlotType_slotTypeValues' - A list of @SlotTypeValue@ objects that defines the values that the slot
-- type can take. Each value can have a list of synonyms, additional values
-- that help train the machine learning model about the values that it
-- resolves for a slot.
--
-- 'description', 'createSlotType_description' - A description of the slot type. Use the description to help identify the
-- slot type in lists.
--
-- 'slotTypeName', 'createSlotType_slotTypeName' - The name for the slot. A slot type name must be unique within the
-- account.
--
-- 'valueSelectionSetting', 'createSlotType_valueSelectionSetting' - Determines the strategy that Amazon Lex uses to select a value from the
-- list of possible values. The field can be set to one of the following
-- values:
--
-- -   @OriginalValue@ - Returns the value entered by the user, if the user
--     value is similar to the slot value.
--
-- -   @TopResolution@ - If there is a resolution list for the slot, return
--     the first value in the resolution list. If there is no resolution
--     list, return null.
--
-- If you don\'t specify the @valueSelectionSetting@ parameter, the default
-- is @OriginalValue@.
--
-- 'botId', 'createSlotType_botId' - The identifier of the bot associated with this slot type.
--
-- 'botVersion', 'createSlotType_botVersion' - The identifier of the bot version associated with this slot type.
--
-- 'localeId', 'createSlotType_localeId' - The identifier of the language and locale that the slot type will be
-- used in. The string must match one of the supported locales. All of the
-- bots, intents, and slots used by the slot type must have the same
-- locale. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newCreateSlotType ::
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
  CreateSlotType
newCreateSlotType
  pSlotTypeName_
  pValueSelectionSetting_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    CreateSlotType'
      { parentSlotTypeSignature =
          Prelude.Nothing,
        slotTypeValues = Prelude.Nothing,
        description = Prelude.Nothing,
        slotTypeName = pSlotTypeName_,
        valueSelectionSetting = pValueSelectionSetting_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The built-in slot type used as a parent of this slot type. When you
-- define a parent slot type, the new slot type has the configuration of
-- the parent slot type.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
createSlotType_parentSlotTypeSignature :: Lens.Lens' CreateSlotType (Prelude.Maybe Prelude.Text)
createSlotType_parentSlotTypeSignature = Lens.lens (\CreateSlotType' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotType' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotType)

-- | A list of @SlotTypeValue@ objects that defines the values that the slot
-- type can take. Each value can have a list of synonyms, additional values
-- that help train the machine learning model about the values that it
-- resolves for a slot.
createSlotType_slotTypeValues :: Lens.Lens' CreateSlotType (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
createSlotType_slotTypeValues = Lens.lens (\CreateSlotType' {slotTypeValues} -> slotTypeValues) (\s@CreateSlotType' {} a -> s {slotTypeValues = a} :: CreateSlotType) Prelude.. Lens.mapping Lens.coerced

-- | A description of the slot type. Use the description to help identify the
-- slot type in lists.
createSlotType_description :: Lens.Lens' CreateSlotType (Prelude.Maybe Prelude.Text)
createSlotType_description = Lens.lens (\CreateSlotType' {description} -> description) (\s@CreateSlotType' {} a -> s {description = a} :: CreateSlotType)

-- | The name for the slot. A slot type name must be unique within the
-- account.
createSlotType_slotTypeName :: Lens.Lens' CreateSlotType Prelude.Text
createSlotType_slotTypeName = Lens.lens (\CreateSlotType' {slotTypeName} -> slotTypeName) (\s@CreateSlotType' {} a -> s {slotTypeName = a} :: CreateSlotType)

-- | Determines the strategy that Amazon Lex uses to select a value from the
-- list of possible values. The field can be set to one of the following
-- values:
--
-- -   @OriginalValue@ - Returns the value entered by the user, if the user
--     value is similar to the slot value.
--
-- -   @TopResolution@ - If there is a resolution list for the slot, return
--     the first value in the resolution list. If there is no resolution
--     list, return null.
--
-- If you don\'t specify the @valueSelectionSetting@ parameter, the default
-- is @OriginalValue@.
createSlotType_valueSelectionSetting :: Lens.Lens' CreateSlotType SlotValueSelectionSetting
createSlotType_valueSelectionSetting = Lens.lens (\CreateSlotType' {valueSelectionSetting} -> valueSelectionSetting) (\s@CreateSlotType' {} a -> s {valueSelectionSetting = a} :: CreateSlotType)

-- | The identifier of the bot associated with this slot type.
createSlotType_botId :: Lens.Lens' CreateSlotType Prelude.Text
createSlotType_botId = Lens.lens (\CreateSlotType' {botId} -> botId) (\s@CreateSlotType' {} a -> s {botId = a} :: CreateSlotType)

-- | The identifier of the bot version associated with this slot type.
createSlotType_botVersion :: Lens.Lens' CreateSlotType Prelude.Text
createSlotType_botVersion = Lens.lens (\CreateSlotType' {botVersion} -> botVersion) (\s@CreateSlotType' {} a -> s {botVersion = a} :: CreateSlotType)

-- | The identifier of the language and locale that the slot type will be
-- used in. The string must match one of the supported locales. All of the
-- bots, intents, and slots used by the slot type must have the same
-- locale. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
createSlotType_localeId :: Lens.Lens' CreateSlotType Prelude.Text
createSlotType_localeId = Lens.lens (\CreateSlotType' {localeId} -> localeId) (\s@CreateSlotType' {} a -> s {localeId = a} :: CreateSlotType)

instance Core.AWSRequest CreateSlotType where
  type
    AWSResponse CreateSlotType =
      CreateSlotTypeResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSlotTypeResponse'
            Prelude.<$> (x Core..?> "parentSlotTypeSignature")
            Prelude.<*> (x Core..?> "slotTypeValues")
            Prelude.<*> (x Core..?> "valueSelectionSetting")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "slotTypeName")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "slotTypeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlotType

instance Prelude.NFData CreateSlotType

instance Core.ToHeaders CreateSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSlotType where
  toJSON CreateSlotType' {..} =
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

instance Core.ToPath CreateSlotType where
  toPath CreateSlotType' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/slottypes/"
      ]

instance Core.ToQuery CreateSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlotTypeResponse' smart constructor.
data CreateSlotTypeResponse = CreateSlotTypeResponse'
  { -- | The signature of the base slot type specified for the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The list of values that the slot type can assume.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The strategy that Amazon Lex uses to select a value from the list of
    -- possible values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the bot associated with the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The specified language and local specified for the slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name specified for the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description specified for the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the slot type. Use this to identify
    -- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'createSlotTypeResponse_parentSlotTypeSignature' - The signature of the base slot type specified for the slot type.
--
-- 'slotTypeValues', 'createSlotTypeResponse_slotTypeValues' - The list of values that the slot type can assume.
--
-- 'valueSelectionSetting', 'createSlotTypeResponse_valueSelectionSetting' - The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
--
-- 'botVersion', 'createSlotTypeResponse_botVersion' - The version of the bot associated with the slot type.
--
-- 'botId', 'createSlotTypeResponse_botId' - The identifier for the bot associated with the slot type.
--
-- 'localeId', 'createSlotTypeResponse_localeId' - The specified language and local specified for the slot type.
--
-- 'creationDateTime', 'createSlotTypeResponse_creationDateTime' - A timestamp of the date and time that the slot type was created.
--
-- 'slotTypeName', 'createSlotTypeResponse_slotTypeName' - The name specified for the slot type.
--
-- 'description', 'createSlotTypeResponse_description' - The description specified for the slot type.
--
-- 'slotTypeId', 'createSlotTypeResponse_slotTypeId' - The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
--
-- 'httpStatus', 'createSlotTypeResponse_httpStatus' - The response's http status code.
newCreateSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotTypeResponse
newCreateSlotTypeResponse pHttpStatus_ =
  CreateSlotTypeResponse'
    { parentSlotTypeSignature =
        Prelude.Nothing,
      slotTypeValues = Prelude.Nothing,
      valueSelectionSetting = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      description = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signature of the base slot type specified for the slot type.
createSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\CreateSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotTypeResponse)

-- | The list of values that the slot type can assume.
createSlotTypeResponse_slotTypeValues :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
createSlotTypeResponse_slotTypeValues = Lens.lens (\CreateSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeValues = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
createSlotTypeResponse_valueSelectionSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
createSlotTypeResponse_valueSelectionSetting = Lens.lens (\CreateSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@CreateSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: CreateSlotTypeResponse)

-- | The version of the bot associated with the slot type.
createSlotTypeResponse_botVersion :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botVersion = Lens.lens (\CreateSlotTypeResponse' {botVersion} -> botVersion) (\s@CreateSlotTypeResponse' {} a -> s {botVersion = a} :: CreateSlotTypeResponse)

-- | The identifier for the bot associated with the slot type.
createSlotTypeResponse_botId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botId = Lens.lens (\CreateSlotTypeResponse' {botId} -> botId) (\s@CreateSlotTypeResponse' {} a -> s {botId = a} :: CreateSlotTypeResponse)

-- | The specified language and local specified for the slot type.
createSlotTypeResponse_localeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_localeId = Lens.lens (\CreateSlotTypeResponse' {localeId} -> localeId) (\s@CreateSlotTypeResponse' {} a -> s {localeId = a} :: CreateSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was created.
createSlotTypeResponse_creationDateTime :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
createSlotTypeResponse_creationDateTime = Lens.lens (\CreateSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@CreateSlotTypeResponse' {} a -> s {creationDateTime = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The name specified for the slot type.
createSlotTypeResponse_slotTypeName :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeName = Lens.lens (\CreateSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeName = a} :: CreateSlotTypeResponse)

-- | The description specified for the slot type.
createSlotTypeResponse_description :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_description = Lens.lens (\CreateSlotTypeResponse' {description} -> description) (\s@CreateSlotTypeResponse' {} a -> s {description = a} :: CreateSlotTypeResponse)

-- | The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
createSlotTypeResponse_slotTypeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeId = Lens.lens (\CreateSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeId = a} :: CreateSlotTypeResponse)

-- | The response's http status code.
createSlotTypeResponse_httpStatus :: Lens.Lens' CreateSlotTypeResponse Prelude.Int
createSlotTypeResponse_httpStatus = Lens.lens (\CreateSlotTypeResponse' {httpStatus} -> httpStatus) (\s@CreateSlotTypeResponse' {} a -> s {httpStatus = a} :: CreateSlotTypeResponse)

instance Prelude.NFData CreateSlotTypeResponse
