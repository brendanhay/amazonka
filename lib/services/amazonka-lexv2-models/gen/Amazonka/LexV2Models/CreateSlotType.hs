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
-- Module      : Amazonka.LexV2Models.CreateSlotType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom slot type
--
-- To create a custom slot type, specify a name for the slot type and a set
-- of enumeration values, the values that a slot of this type can assume.
module Amazonka.LexV2Models.CreateSlotType
  ( -- * Creating a Request
    CreateSlotType (..),
    newCreateSlotType,

    -- * Request Lenses
    createSlotType_compositeSlotTypeSetting,
    createSlotType_externalSourceSetting,
    createSlotType_valueSelectionSetting,
    createSlotType_description,
    createSlotType_slotTypeValues,
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeName,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,

    -- * Destructuring the Response
    CreateSlotTypeResponse (..),
    newCreateSlotTypeResponse,

    -- * Response Lenses
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_compositeSlotTypeSetting,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_externalSourceSetting,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_description,
    createSlotTypeResponse_botId,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSlotType' smart constructor.
data CreateSlotType = CreateSlotType'
  { -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | Sets the type of external information used to create the slot type.
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
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
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | A description of the slot type. Use the description to help identify the
    -- slot type in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @SlotTypeValue@ objects that defines the values that the slot
    -- type can take. Each value can have a list of synonyms, additional values
    -- that help train the machine learning model about the values that it
    -- resolves for a slot.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The built-in slot type used as a parent of this slot type. When you
    -- define a parent slot type, the new slot type has the configuration of
    -- the parent slot type.
    --
    -- Only @AMAZON.AlphaNumeric@ is supported.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The name for the slot. A slot type name must be unique within the
    -- account.
    slotTypeName :: Prelude.Text,
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
-- 'compositeSlotTypeSetting', 'createSlotType_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'externalSourceSetting', 'createSlotType_externalSourceSetting' - Sets the type of external information used to create the slot type.
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
-- 'description', 'createSlotType_description' - A description of the slot type. Use the description to help identify the
-- slot type in lists.
--
-- 'slotTypeValues', 'createSlotType_slotTypeValues' - A list of @SlotTypeValue@ objects that defines the values that the slot
-- type can take. Each value can have a list of synonyms, additional values
-- that help train the machine learning model about the values that it
-- resolves for a slot.
--
-- 'parentSlotTypeSignature', 'createSlotType_parentSlotTypeSignature' - The built-in slot type used as a parent of this slot type. When you
-- define a parent slot type, the new slot type has the configuration of
-- the parent slot type.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
--
-- 'slotTypeName', 'createSlotType_slotTypeName' - The name for the slot. A slot type name must be unique within the
-- account.
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
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  CreateSlotType
newCreateSlotType
  pSlotTypeName_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    CreateSlotType'
      { compositeSlotTypeSetting =
          Prelude.Nothing,
        externalSourceSetting = Prelude.Nothing,
        valueSelectionSetting = Prelude.Nothing,
        description = Prelude.Nothing,
        slotTypeValues = Prelude.Nothing,
        parentSlotTypeSignature = Prelude.Nothing,
        slotTypeName = pSlotTypeName_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | Specifications for a composite slot type.
createSlotType_compositeSlotTypeSetting :: Lens.Lens' CreateSlotType (Prelude.Maybe CompositeSlotTypeSetting)
createSlotType_compositeSlotTypeSetting = Lens.lens (\CreateSlotType' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@CreateSlotType' {} a -> s {compositeSlotTypeSetting = a} :: CreateSlotType)

-- | Sets the type of external information used to create the slot type.
createSlotType_externalSourceSetting :: Lens.Lens' CreateSlotType (Prelude.Maybe ExternalSourceSetting)
createSlotType_externalSourceSetting = Lens.lens (\CreateSlotType' {externalSourceSetting} -> externalSourceSetting) (\s@CreateSlotType' {} a -> s {externalSourceSetting = a} :: CreateSlotType)

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
createSlotType_valueSelectionSetting :: Lens.Lens' CreateSlotType (Prelude.Maybe SlotValueSelectionSetting)
createSlotType_valueSelectionSetting = Lens.lens (\CreateSlotType' {valueSelectionSetting} -> valueSelectionSetting) (\s@CreateSlotType' {} a -> s {valueSelectionSetting = a} :: CreateSlotType)

-- | A description of the slot type. Use the description to help identify the
-- slot type in lists.
createSlotType_description :: Lens.Lens' CreateSlotType (Prelude.Maybe Prelude.Text)
createSlotType_description = Lens.lens (\CreateSlotType' {description} -> description) (\s@CreateSlotType' {} a -> s {description = a} :: CreateSlotType)

-- | A list of @SlotTypeValue@ objects that defines the values that the slot
-- type can take. Each value can have a list of synonyms, additional values
-- that help train the machine learning model about the values that it
-- resolves for a slot.
createSlotType_slotTypeValues :: Lens.Lens' CreateSlotType (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
createSlotType_slotTypeValues = Lens.lens (\CreateSlotType' {slotTypeValues} -> slotTypeValues) (\s@CreateSlotType' {} a -> s {slotTypeValues = a} :: CreateSlotType) Prelude.. Lens.mapping Lens.coerced

-- | The built-in slot type used as a parent of this slot type. When you
-- define a parent slot type, the new slot type has the configuration of
-- the parent slot type.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
createSlotType_parentSlotTypeSignature :: Lens.Lens' CreateSlotType (Prelude.Maybe Prelude.Text)
createSlotType_parentSlotTypeSignature = Lens.lens (\CreateSlotType' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotType' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotType)

-- | The name for the slot. A slot type name must be unique within the
-- account.
createSlotType_slotTypeName :: Lens.Lens' CreateSlotType Prelude.Text
createSlotType_slotTypeName = Lens.lens (\CreateSlotType' {slotTypeName} -> slotTypeName) (\s@CreateSlotType' {} a -> s {slotTypeName = a} :: CreateSlotType)

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSlotTypeResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "compositeSlotTypeSetting")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "externalSourceSetting")
            Prelude.<*> (x Core..?> "valueSelectionSetting")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "slotTypeValues")
            Prelude.<*> (x Core..?> "slotTypeName")
            Prelude.<*> (x Core..?> "slotTypeId")
            Prelude.<*> (x Core..?> "parentSlotTypeSignature")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlotType where
  hashWithSalt _salt CreateSlotType' {..} =
    _salt
      `Prelude.hashWithSalt` compositeSlotTypeSetting
      `Prelude.hashWithSalt` externalSourceSetting
      `Prelude.hashWithSalt` valueSelectionSetting
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` slotTypeValues
      `Prelude.hashWithSalt` parentSlotTypeSignature
      `Prelude.hashWithSalt` slotTypeName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData CreateSlotType where
  rnf CreateSlotType' {..} =
    Prelude.rnf compositeSlotTypeSetting
      `Prelude.seq` Prelude.rnf externalSourceSetting
      `Prelude.seq` Prelude.rnf valueSelectionSetting
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf slotTypeValues
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf slotTypeName
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

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
          [ ("compositeSlotTypeSetting" Core..=)
              Prelude.<$> compositeSlotTypeSetting,
            ("externalSourceSetting" Core..=)
              Prelude.<$> externalSourceSetting,
            ("valueSelectionSetting" Core..=)
              Prelude.<$> valueSelectionSetting,
            ("description" Core..=) Prelude.<$> description,
            ("slotTypeValues" Core..=)
              Prelude.<$> slotTypeValues,
            ("parentSlotTypeSignature" Core..=)
              Prelude.<$> parentSlotTypeSignature,
            Prelude.Just ("slotTypeName" Core..= slotTypeName)
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
  { -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | A timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The specified language and local specified for the slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The type of external information used to create the slot type.
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | The strategy that Amazon Lex uses to select a value from the list of
    -- possible values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The description specified for the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the bot associated with the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The list of values that the slot type can assume.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The name specified for the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the slot type. Use this to identify
    -- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The signature of the base slot type specified for the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
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
-- 'botVersion', 'createSlotTypeResponse_botVersion' - The version of the bot associated with the slot type.
--
-- 'compositeSlotTypeSetting', 'createSlotTypeResponse_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'creationDateTime', 'createSlotTypeResponse_creationDateTime' - A timestamp of the date and time that the slot type was created.
--
-- 'localeId', 'createSlotTypeResponse_localeId' - The specified language and local specified for the slot type.
--
-- 'externalSourceSetting', 'createSlotTypeResponse_externalSourceSetting' - The type of external information used to create the slot type.
--
-- 'valueSelectionSetting', 'createSlotTypeResponse_valueSelectionSetting' - The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
--
-- 'description', 'createSlotTypeResponse_description' - The description specified for the slot type.
--
-- 'botId', 'createSlotTypeResponse_botId' - The identifier for the bot associated with the slot type.
--
-- 'slotTypeValues', 'createSlotTypeResponse_slotTypeValues' - The list of values that the slot type can assume.
--
-- 'slotTypeName', 'createSlotTypeResponse_slotTypeName' - The name specified for the slot type.
--
-- 'slotTypeId', 'createSlotTypeResponse_slotTypeId' - The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
--
-- 'parentSlotTypeSignature', 'createSlotTypeResponse_parentSlotTypeSignature' - The signature of the base slot type specified for the slot type.
--
-- 'httpStatus', 'createSlotTypeResponse_httpStatus' - The response's http status code.
newCreateSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotTypeResponse
newCreateSlotTypeResponse pHttpStatus_ =
  CreateSlotTypeResponse'
    { botVersion =
        Prelude.Nothing,
      compositeSlotTypeSetting = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      localeId = Prelude.Nothing,
      externalSourceSetting = Prelude.Nothing,
      valueSelectionSetting = Prelude.Nothing,
      description = Prelude.Nothing,
      botId = Prelude.Nothing,
      slotTypeValues = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot associated with the slot type.
createSlotTypeResponse_botVersion :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botVersion = Lens.lens (\CreateSlotTypeResponse' {botVersion} -> botVersion) (\s@CreateSlotTypeResponse' {} a -> s {botVersion = a} :: CreateSlotTypeResponse)

-- | Specifications for a composite slot type.
createSlotTypeResponse_compositeSlotTypeSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe CompositeSlotTypeSetting)
createSlotTypeResponse_compositeSlotTypeSetting = Lens.lens (\CreateSlotTypeResponse' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@CreateSlotTypeResponse' {} a -> s {compositeSlotTypeSetting = a} :: CreateSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was created.
createSlotTypeResponse_creationDateTime :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
createSlotTypeResponse_creationDateTime = Lens.lens (\CreateSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@CreateSlotTypeResponse' {} a -> s {creationDateTime = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The specified language and local specified for the slot type.
createSlotTypeResponse_localeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_localeId = Lens.lens (\CreateSlotTypeResponse' {localeId} -> localeId) (\s@CreateSlotTypeResponse' {} a -> s {localeId = a} :: CreateSlotTypeResponse)

-- | The type of external information used to create the slot type.
createSlotTypeResponse_externalSourceSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe ExternalSourceSetting)
createSlotTypeResponse_externalSourceSetting = Lens.lens (\CreateSlotTypeResponse' {externalSourceSetting} -> externalSourceSetting) (\s@CreateSlotTypeResponse' {} a -> s {externalSourceSetting = a} :: CreateSlotTypeResponse)

-- | The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
createSlotTypeResponse_valueSelectionSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
createSlotTypeResponse_valueSelectionSetting = Lens.lens (\CreateSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@CreateSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: CreateSlotTypeResponse)

-- | The description specified for the slot type.
createSlotTypeResponse_description :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_description = Lens.lens (\CreateSlotTypeResponse' {description} -> description) (\s@CreateSlotTypeResponse' {} a -> s {description = a} :: CreateSlotTypeResponse)

-- | The identifier for the bot associated with the slot type.
createSlotTypeResponse_botId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botId = Lens.lens (\CreateSlotTypeResponse' {botId} -> botId) (\s@CreateSlotTypeResponse' {} a -> s {botId = a} :: CreateSlotTypeResponse)

-- | The list of values that the slot type can assume.
createSlotTypeResponse_slotTypeValues :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
createSlotTypeResponse_slotTypeValues = Lens.lens (\CreateSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeValues = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name specified for the slot type.
createSlotTypeResponse_slotTypeName :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeName = Lens.lens (\CreateSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeName = a} :: CreateSlotTypeResponse)

-- | The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
createSlotTypeResponse_slotTypeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeId = Lens.lens (\CreateSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeId = a} :: CreateSlotTypeResponse)

-- | The signature of the base slot type specified for the slot type.
createSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\CreateSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotTypeResponse)

-- | The response's http status code.
createSlotTypeResponse_httpStatus :: Lens.Lens' CreateSlotTypeResponse Prelude.Int
createSlotTypeResponse_httpStatus = Lens.lens (\CreateSlotTypeResponse' {httpStatus} -> httpStatus) (\s@CreateSlotTypeResponse' {} a -> s {httpStatus = a} :: CreateSlotTypeResponse)

instance Prelude.NFData CreateSlotTypeResponse where
  rnf CreateSlotTypeResponse' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf compositeSlotTypeSetting
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf externalSourceSetting
      `Prelude.seq` Prelude.rnf valueSelectionSetting
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf slotTypeValues
      `Prelude.seq` Prelude.rnf slotTypeName
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf httpStatus
