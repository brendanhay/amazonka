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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createSlotType_description,
    createSlotType_externalSourceSetting,
    createSlotType_parentSlotTypeSignature,
    createSlotType_slotTypeValues,
    createSlotType_valueSelectionSetting,
    createSlotType_slotTypeName,
    createSlotType_botId,
    createSlotType_botVersion,
    createSlotType_localeId,

    -- * Destructuring the Response
    CreateSlotTypeResponse (..),
    newCreateSlotTypeResponse,

    -- * Response Lenses
    createSlotTypeResponse_botId,
    createSlotTypeResponse_botVersion,
    createSlotTypeResponse_compositeSlotTypeSetting,
    createSlotTypeResponse_creationDateTime,
    createSlotTypeResponse_description,
    createSlotTypeResponse_externalSourceSetting,
    createSlotTypeResponse_localeId,
    createSlotTypeResponse_parentSlotTypeSignature,
    createSlotTypeResponse_slotTypeId,
    createSlotTypeResponse_slotTypeName,
    createSlotTypeResponse_slotTypeValues,
    createSlotTypeResponse_valueSelectionSetting,
    createSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSlotType' smart constructor.
data CreateSlotType = CreateSlotType'
  { -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | A description of the slot type. Use the description to help identify the
    -- slot type in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | Sets the type of external information used to create the slot type.
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | The built-in slot type used as a parent of this slot type. When you
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
-- 'description', 'createSlotType_description' - A description of the slot type. Use the description to help identify the
-- slot type in lists.
--
-- 'externalSourceSetting', 'createSlotType_externalSourceSetting' - Sets the type of external information used to create the slot type.
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
        description = Prelude.Nothing,
        externalSourceSetting = Prelude.Nothing,
        parentSlotTypeSignature = Prelude.Nothing,
        slotTypeValues = Prelude.Nothing,
        valueSelectionSetting = Prelude.Nothing,
        slotTypeName = pSlotTypeName_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | Specifications for a composite slot type.
createSlotType_compositeSlotTypeSetting :: Lens.Lens' CreateSlotType (Prelude.Maybe CompositeSlotTypeSetting)
createSlotType_compositeSlotTypeSetting = Lens.lens (\CreateSlotType' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@CreateSlotType' {} a -> s {compositeSlotTypeSetting = a} :: CreateSlotType)

-- | A description of the slot type. Use the description to help identify the
-- slot type in lists.
createSlotType_description :: Lens.Lens' CreateSlotType (Prelude.Maybe Prelude.Text)
createSlotType_description = Lens.lens (\CreateSlotType' {description} -> description) (\s@CreateSlotType' {} a -> s {description = a} :: CreateSlotType)

-- | Sets the type of external information used to create the slot type.
createSlotType_externalSourceSetting :: Lens.Lens' CreateSlotType (Prelude.Maybe ExternalSourceSetting)
createSlotType_externalSourceSetting = Lens.lens (\CreateSlotType' {externalSourceSetting} -> externalSourceSetting) (\s@CreateSlotType' {} a -> s {externalSourceSetting = a} :: CreateSlotType)

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
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "compositeSlotTypeSetting")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "externalSourceSetting")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "parentSlotTypeSignature")
            Prelude.<*> (x Data..?> "slotTypeId")
            Prelude.<*> (x Data..?> "slotTypeName")
            Prelude.<*> (x Data..?> "slotTypeValues")
            Prelude.<*> (x Data..?> "valueSelectionSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlotType where
  hashWithSalt _salt CreateSlotType' {..} =
    _salt
      `Prelude.hashWithSalt` compositeSlotTypeSetting
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` externalSourceSetting
      `Prelude.hashWithSalt` parentSlotTypeSignature
      `Prelude.hashWithSalt` slotTypeValues
      `Prelude.hashWithSalt` valueSelectionSetting
      `Prelude.hashWithSalt` slotTypeName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData CreateSlotType where
  rnf CreateSlotType' {..} =
    Prelude.rnf compositeSlotTypeSetting `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf externalSourceSetting `Prelude.seq`
          Prelude.rnf parentSlotTypeSignature `Prelude.seq`
            Prelude.rnf slotTypeValues `Prelude.seq`
              Prelude.rnf valueSelectionSetting `Prelude.seq`
                Prelude.rnf slotTypeName `Prelude.seq`
                  Prelude.rnf botId `Prelude.seq`
                    Prelude.rnf botVersion `Prelude.seq`
                      Prelude.rnf localeId

instance Data.ToHeaders CreateSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSlotType where
  toJSON CreateSlotType' {..} =
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

instance Data.ToPath CreateSlotType where
  toPath CreateSlotType' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/slottypes/"
      ]

instance Data.ToQuery CreateSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlotTypeResponse' smart constructor.
data CreateSlotTypeResponse = CreateSlotTypeResponse'
  { -- | The identifier for the bot associated with the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | A timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description specified for the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of external information used to create the slot type.
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | The specified language and local specified for the slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The signature of the base slot type specified for the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the slot type. Use this to identify
    -- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The name specified for the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The list of values that the slot type can assume.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The strategy that Amazon Lex uses to select a value from the list of
    -- possible values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
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
-- 'botId', 'createSlotTypeResponse_botId' - The identifier for the bot associated with the slot type.
--
-- 'botVersion', 'createSlotTypeResponse_botVersion' - The version of the bot associated with the slot type.
--
-- 'compositeSlotTypeSetting', 'createSlotTypeResponse_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'creationDateTime', 'createSlotTypeResponse_creationDateTime' - A timestamp of the date and time that the slot type was created.
--
-- 'description', 'createSlotTypeResponse_description' - The description specified for the slot type.
--
-- 'externalSourceSetting', 'createSlotTypeResponse_externalSourceSetting' - The type of external information used to create the slot type.
--
-- 'localeId', 'createSlotTypeResponse_localeId' - The specified language and local specified for the slot type.
--
-- 'parentSlotTypeSignature', 'createSlotTypeResponse_parentSlotTypeSignature' - The signature of the base slot type specified for the slot type.
--
-- 'slotTypeId', 'createSlotTypeResponse_slotTypeId' - The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
--
-- 'slotTypeName', 'createSlotTypeResponse_slotTypeName' - The name specified for the slot type.
--
-- 'slotTypeValues', 'createSlotTypeResponse_slotTypeValues' - The list of values that the slot type can assume.
--
-- 'valueSelectionSetting', 'createSlotTypeResponse_valueSelectionSetting' - The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
--
-- 'httpStatus', 'createSlotTypeResponse_httpStatus' - The response's http status code.
newCreateSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotTypeResponse
newCreateSlotTypeResponse pHttpStatus_ =
  CreateSlotTypeResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      compositeSlotTypeSetting = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      externalSourceSetting = Prelude.Nothing,
      localeId = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      slotTypeValues = Prelude.Nothing,
      valueSelectionSetting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the bot associated with the slot type.
createSlotTypeResponse_botId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botId = Lens.lens (\CreateSlotTypeResponse' {botId} -> botId) (\s@CreateSlotTypeResponse' {} a -> s {botId = a} :: CreateSlotTypeResponse)

-- | The version of the bot associated with the slot type.
createSlotTypeResponse_botVersion :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_botVersion = Lens.lens (\CreateSlotTypeResponse' {botVersion} -> botVersion) (\s@CreateSlotTypeResponse' {} a -> s {botVersion = a} :: CreateSlotTypeResponse)

-- | Specifications for a composite slot type.
createSlotTypeResponse_compositeSlotTypeSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe CompositeSlotTypeSetting)
createSlotTypeResponse_compositeSlotTypeSetting = Lens.lens (\CreateSlotTypeResponse' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@CreateSlotTypeResponse' {} a -> s {compositeSlotTypeSetting = a} :: CreateSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was created.
createSlotTypeResponse_creationDateTime :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
createSlotTypeResponse_creationDateTime = Lens.lens (\CreateSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@CreateSlotTypeResponse' {} a -> s {creationDateTime = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The description specified for the slot type.
createSlotTypeResponse_description :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_description = Lens.lens (\CreateSlotTypeResponse' {description} -> description) (\s@CreateSlotTypeResponse' {} a -> s {description = a} :: CreateSlotTypeResponse)

-- | The type of external information used to create the slot type.
createSlotTypeResponse_externalSourceSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe ExternalSourceSetting)
createSlotTypeResponse_externalSourceSetting = Lens.lens (\CreateSlotTypeResponse' {externalSourceSetting} -> externalSourceSetting) (\s@CreateSlotTypeResponse' {} a -> s {externalSourceSetting = a} :: CreateSlotTypeResponse)

-- | The specified language and local specified for the slot type.
createSlotTypeResponse_localeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_localeId = Lens.lens (\CreateSlotTypeResponse' {localeId} -> localeId) (\s@CreateSlotTypeResponse' {} a -> s {localeId = a} :: CreateSlotTypeResponse)

-- | The signature of the base slot type specified for the slot type.
createSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\CreateSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotTypeResponse)

-- | The unique identifier assigned to the slot type. Use this to identify
-- the slot type in the @UpdateSlotType@ and @DeleteSlotType@ operations.
createSlotTypeResponse_slotTypeId :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeId = Lens.lens (\CreateSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeId = a} :: CreateSlotTypeResponse)

-- | The name specified for the slot type.
createSlotTypeResponse_slotTypeName :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe Prelude.Text)
createSlotTypeResponse_slotTypeName = Lens.lens (\CreateSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeName = a} :: CreateSlotTypeResponse)

-- | The list of values that the slot type can assume.
createSlotTypeResponse_slotTypeValues :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
createSlotTypeResponse_slotTypeValues = Lens.lens (\CreateSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@CreateSlotTypeResponse' {} a -> s {slotTypeValues = a} :: CreateSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The strategy that Amazon Lex uses to select a value from the list of
-- possible values.
createSlotTypeResponse_valueSelectionSetting :: Lens.Lens' CreateSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
createSlotTypeResponse_valueSelectionSetting = Lens.lens (\CreateSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@CreateSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: CreateSlotTypeResponse)

-- | The response's http status code.
createSlotTypeResponse_httpStatus :: Lens.Lens' CreateSlotTypeResponse Prelude.Int
createSlotTypeResponse_httpStatus = Lens.lens (\CreateSlotTypeResponse' {httpStatus} -> httpStatus) (\s@CreateSlotTypeResponse' {} a -> s {httpStatus = a} :: CreateSlotTypeResponse)

instance Prelude.NFData CreateSlotTypeResponse where
  rnf CreateSlotTypeResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf compositeSlotTypeSetting `Prelude.seq`
          Prelude.rnf creationDateTime `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf externalSourceSetting `Prelude.seq`
                Prelude.rnf localeId `Prelude.seq`
                  Prelude.rnf parentSlotTypeSignature `Prelude.seq`
                    Prelude.rnf slotTypeId `Prelude.seq`
                      Prelude.rnf slotTypeName `Prelude.seq`
                        Prelude.rnf slotTypeValues `Prelude.seq`
                          Prelude.rnf valueSelectionSetting `Prelude.seq`
                            Prelude.rnf httpStatus
