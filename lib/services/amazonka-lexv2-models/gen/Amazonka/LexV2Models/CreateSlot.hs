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
-- Module      : Amazonka.LexV2Models.CreateSlot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a slot in an intent. A slot is a variable needed to fulfill an
-- intent. For example, an @OrderPizza@ intent might need slots for size,
-- crust, and number of pizzas. For each slot, you define one or more
-- utterances that Amazon Lex uses to elicit a response from the user.
module Amazonka.LexV2Models.CreateSlot
  ( -- * Creating a Request
    CreateSlot (..),
    newCreateSlot,

    -- * Request Lenses
    createSlot_description,
    createSlot_multipleValuesSetting,
    createSlot_obfuscationSetting,
    createSlot_slotTypeId,
    createSlot_subSlotSetting,
    createSlot_slotName,
    createSlot_valueElicitationSetting,
    createSlot_botId,
    createSlot_botVersion,
    createSlot_localeId,
    createSlot_intentId,

    -- * Destructuring the Response
    CreateSlotResponse (..),
    newCreateSlotResponse,

    -- * Response Lenses
    createSlotResponse_botId,
    createSlotResponse_botVersion,
    createSlotResponse_creationDateTime,
    createSlotResponse_description,
    createSlotResponse_intentId,
    createSlotResponse_localeId,
    createSlotResponse_multipleValuesSetting,
    createSlotResponse_obfuscationSetting,
    createSlotResponse_slotId,
    createSlotResponse_slotName,
    createSlotResponse_slotTypeId,
    createSlotResponse_subSlotSetting,
    createSlotResponse_valueElicitationSetting,
    createSlotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSlot' smart constructor.
data CreateSlot = CreateSlot'
  { -- | A description of the slot. Use this to help identify the slot in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the slot returns multiple values in one response.
    -- Multi-value slots are only available in the en-US locale. If you set
    -- this value to @true@ in any other locale, Amazon Lex throws a
    -- @ValidationException@.
    --
    -- If the @multipleValuesSetting@ is not set, the default value is @false@.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | Determines how slot values are used in Amazon CloudWatch logs. If the
    -- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
    -- slot values are obfuscated in the log output. If the value is @None@,
    -- the actual value is present in the log output.
    --
    -- The default is to obfuscate values in the CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | The unique identifier for the slot type associated with this slot. The
    -- slot type determines the values that can be entered into the slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | Specifications for the constituent sub slots and the expression for the
    -- composite slot.
    subSlotSetting :: Prelude.Maybe SubSlotSetting,
    -- | The name of the slot. Slot names must be unique within the bot that
    -- contains the slot.
    slotName :: Prelude.Text,
    -- | Specifies prompts that Amazon Lex sends to the user to elicit a response
    -- that provides the value for the slot.
    valueElicitationSetting :: SlotValueElicitationSetting,
    -- | The identifier of the bot associated with the slot.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the slot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the slot will be used in.
    -- The string must match one of the supported locales. All of the bots,
    -- intents, slot types used by the slot must have the same locale. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The identifier of the intent that contains the slot.
    intentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSlot_description' - A description of the slot. Use this to help identify the slot in lists.
--
-- 'multipleValuesSetting', 'createSlot_multipleValuesSetting' - Indicates whether the slot returns multiple values in one response.
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
--
-- 'obfuscationSetting', 'createSlot_obfuscationSetting' - Determines how slot values are used in Amazon CloudWatch logs. If the
-- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
-- slot values are obfuscated in the log output. If the value is @None@,
-- the actual value is present in the log output.
--
-- The default is to obfuscate values in the CloudWatch logs.
--
-- 'slotTypeId', 'createSlot_slotTypeId' - The unique identifier for the slot type associated with this slot. The
-- slot type determines the values that can be entered into the slot.
--
-- 'subSlotSetting', 'createSlot_subSlotSetting' - Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- 'slotName', 'createSlot_slotName' - The name of the slot. Slot names must be unique within the bot that
-- contains the slot.
--
-- 'valueElicitationSetting', 'createSlot_valueElicitationSetting' - Specifies prompts that Amazon Lex sends to the user to elicit a response
-- that provides the value for the slot.
--
-- 'botId', 'createSlot_botId' - The identifier of the bot associated with the slot.
--
-- 'botVersion', 'createSlot_botVersion' - The version of the bot associated with the slot.
--
-- 'localeId', 'createSlot_localeId' - The identifier of the language and locale that the slot will be used in.
-- The string must match one of the supported locales. All of the bots,
-- intents, slot types used by the slot must have the same locale. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'intentId', 'createSlot_intentId' - The identifier of the intent that contains the slot.
newCreateSlot ::
  -- | 'slotName'
  Prelude.Text ->
  -- | 'valueElicitationSetting'
  SlotValueElicitationSetting ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'intentId'
  Prelude.Text ->
  CreateSlot
newCreateSlot
  pSlotName_
  pValueElicitationSetting_
  pBotId_
  pBotVersion_
  pLocaleId_
  pIntentId_ =
    CreateSlot'
      { description = Prelude.Nothing,
        multipleValuesSetting = Prelude.Nothing,
        obfuscationSetting = Prelude.Nothing,
        slotTypeId = Prelude.Nothing,
        subSlotSetting = Prelude.Nothing,
        slotName = pSlotName_,
        valueElicitationSetting = pValueElicitationSetting_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | A description of the slot. Use this to help identify the slot in lists.
createSlot_description :: Lens.Lens' CreateSlot (Prelude.Maybe Prelude.Text)
createSlot_description = Lens.lens (\CreateSlot' {description} -> description) (\s@CreateSlot' {} a -> s {description = a} :: CreateSlot)

-- | Indicates whether the slot returns multiple values in one response.
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
createSlot_multipleValuesSetting :: Lens.Lens' CreateSlot (Prelude.Maybe MultipleValuesSetting)
createSlot_multipleValuesSetting = Lens.lens (\CreateSlot' {multipleValuesSetting} -> multipleValuesSetting) (\s@CreateSlot' {} a -> s {multipleValuesSetting = a} :: CreateSlot)

-- | Determines how slot values are used in Amazon CloudWatch logs. If the
-- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
-- slot values are obfuscated in the log output. If the value is @None@,
-- the actual value is present in the log output.
--
-- The default is to obfuscate values in the CloudWatch logs.
createSlot_obfuscationSetting :: Lens.Lens' CreateSlot (Prelude.Maybe ObfuscationSetting)
createSlot_obfuscationSetting = Lens.lens (\CreateSlot' {obfuscationSetting} -> obfuscationSetting) (\s@CreateSlot' {} a -> s {obfuscationSetting = a} :: CreateSlot)

-- | The unique identifier for the slot type associated with this slot. The
-- slot type determines the values that can be entered into the slot.
createSlot_slotTypeId :: Lens.Lens' CreateSlot (Prelude.Maybe Prelude.Text)
createSlot_slotTypeId = Lens.lens (\CreateSlot' {slotTypeId} -> slotTypeId) (\s@CreateSlot' {} a -> s {slotTypeId = a} :: CreateSlot)

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
createSlot_subSlotSetting :: Lens.Lens' CreateSlot (Prelude.Maybe SubSlotSetting)
createSlot_subSlotSetting = Lens.lens (\CreateSlot' {subSlotSetting} -> subSlotSetting) (\s@CreateSlot' {} a -> s {subSlotSetting = a} :: CreateSlot)

-- | The name of the slot. Slot names must be unique within the bot that
-- contains the slot.
createSlot_slotName :: Lens.Lens' CreateSlot Prelude.Text
createSlot_slotName = Lens.lens (\CreateSlot' {slotName} -> slotName) (\s@CreateSlot' {} a -> s {slotName = a} :: CreateSlot)

-- | Specifies prompts that Amazon Lex sends to the user to elicit a response
-- that provides the value for the slot.
createSlot_valueElicitationSetting :: Lens.Lens' CreateSlot SlotValueElicitationSetting
createSlot_valueElicitationSetting = Lens.lens (\CreateSlot' {valueElicitationSetting} -> valueElicitationSetting) (\s@CreateSlot' {} a -> s {valueElicitationSetting = a} :: CreateSlot)

-- | The identifier of the bot associated with the slot.
createSlot_botId :: Lens.Lens' CreateSlot Prelude.Text
createSlot_botId = Lens.lens (\CreateSlot' {botId} -> botId) (\s@CreateSlot' {} a -> s {botId = a} :: CreateSlot)

-- | The version of the bot associated with the slot.
createSlot_botVersion :: Lens.Lens' CreateSlot Prelude.Text
createSlot_botVersion = Lens.lens (\CreateSlot' {botVersion} -> botVersion) (\s@CreateSlot' {} a -> s {botVersion = a} :: CreateSlot)

-- | The identifier of the language and locale that the slot will be used in.
-- The string must match one of the supported locales. All of the bots,
-- intents, slot types used by the slot must have the same locale. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
createSlot_localeId :: Lens.Lens' CreateSlot Prelude.Text
createSlot_localeId = Lens.lens (\CreateSlot' {localeId} -> localeId) (\s@CreateSlot' {} a -> s {localeId = a} :: CreateSlot)

-- | The identifier of the intent that contains the slot.
createSlot_intentId :: Lens.Lens' CreateSlot Prelude.Text
createSlot_intentId = Lens.lens (\CreateSlot' {intentId} -> intentId) (\s@CreateSlot' {} a -> s {intentId = a} :: CreateSlot)

instance Core.AWSRequest CreateSlot where
  type AWSResponse CreateSlot = CreateSlotResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSlotResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "intentId")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "multipleValuesSetting")
            Prelude.<*> (x Data..?> "obfuscationSetting")
            Prelude.<*> (x Data..?> "slotId")
            Prelude.<*> (x Data..?> "slotName")
            Prelude.<*> (x Data..?> "slotTypeId")
            Prelude.<*> (x Data..?> "subSlotSetting")
            Prelude.<*> (x Data..?> "valueElicitationSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlot where
  hashWithSalt _salt CreateSlot' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` multipleValuesSetting
      `Prelude.hashWithSalt` obfuscationSetting
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` subSlotSetting
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` valueElicitationSetting
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` intentId

instance Prelude.NFData CreateSlot where
  rnf CreateSlot' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf multipleValuesSetting
      `Prelude.seq` Prelude.rnf obfuscationSetting
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf subSlotSetting
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf valueElicitationSetting
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf intentId

instance Data.ToHeaders CreateSlot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSlot where
  toJSON CreateSlot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("multipleValuesSetting" Data..=)
              Prelude.<$> multipleValuesSetting,
            ("obfuscationSetting" Data..=)
              Prelude.<$> obfuscationSetting,
            ("slotTypeId" Data..=) Prelude.<$> slotTypeId,
            ("subSlotSetting" Data..=)
              Prelude.<$> subSlotSetting,
            Prelude.Just ("slotName" Data..= slotName),
            Prelude.Just
              ( "valueElicitationSetting"
                  Data..= valueElicitationSetting
              )
          ]
      )

instance Data.ToPath CreateSlot where
  toPath CreateSlot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/intents/",
        Data.toBS intentId,
        "/slots/"
      ]

instance Data.ToQuery CreateSlot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlotResponse' smart constructor.
data CreateSlotResponse = CreateSlotResponse'
  { -- | The unique identifier of the bot associated with the slot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot associated with the slot.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the date and time that the slot was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description associated with the slot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the intent associated with the slot.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The language and local specified for the slot.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the slot returns multiple values in one response.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | Indicates whether the slot is configured to obfuscate values in Amazon
    -- CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | The unique identifier associated with the slot. Use this to identify the
    -- slot when you update or delete it.
    slotId :: Prelude.Maybe Prelude.Text,
    -- | The name specified for the slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the slot type associated with this slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | Specifications for the constituent sub slots and the expression for the
    -- composite slot.
    subSlotSetting :: Prelude.Maybe SubSlotSetting,
    -- | The value elicitation settings specified for the slot.
    valueElicitationSetting :: Prelude.Maybe SlotValueElicitationSetting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'createSlotResponse_botId' - The unique identifier of the bot associated with the slot.
--
-- 'botVersion', 'createSlotResponse_botVersion' - The version of the bot associated with the slot.
--
-- 'creationDateTime', 'createSlotResponse_creationDateTime' - The timestamp of the date and time that the slot was created.
--
-- 'description', 'createSlotResponse_description' - The description associated with the slot.
--
-- 'intentId', 'createSlotResponse_intentId' - The unique identifier of the intent associated with the slot.
--
-- 'localeId', 'createSlotResponse_localeId' - The language and local specified for the slot.
--
-- 'multipleValuesSetting', 'createSlotResponse_multipleValuesSetting' - Indicates whether the slot returns multiple values in one response.
--
-- 'obfuscationSetting', 'createSlotResponse_obfuscationSetting' - Indicates whether the slot is configured to obfuscate values in Amazon
-- CloudWatch logs.
--
-- 'slotId', 'createSlotResponse_slotId' - The unique identifier associated with the slot. Use this to identify the
-- slot when you update or delete it.
--
-- 'slotName', 'createSlotResponse_slotName' - The name specified for the slot.
--
-- 'slotTypeId', 'createSlotResponse_slotTypeId' - The unique identifier of the slot type associated with this slot.
--
-- 'subSlotSetting', 'createSlotResponse_subSlotSetting' - Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- 'valueElicitationSetting', 'createSlotResponse_valueElicitationSetting' - The value elicitation settings specified for the slot.
--
-- 'httpStatus', 'createSlotResponse_httpStatus' - The response's http status code.
newCreateSlotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotResponse
newCreateSlotResponse pHttpStatus_ =
  CreateSlotResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      intentId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      multipleValuesSetting = Prelude.Nothing,
      obfuscationSetting = Prelude.Nothing,
      slotId = Prelude.Nothing,
      slotName = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      subSlotSetting = Prelude.Nothing,
      valueElicitationSetting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot associated with the slot.
createSlotResponse_botId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_botId = Lens.lens (\CreateSlotResponse' {botId} -> botId) (\s@CreateSlotResponse' {} a -> s {botId = a} :: CreateSlotResponse)

-- | The version of the bot associated with the slot.
createSlotResponse_botVersion :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_botVersion = Lens.lens (\CreateSlotResponse' {botVersion} -> botVersion) (\s@CreateSlotResponse' {} a -> s {botVersion = a} :: CreateSlotResponse)

-- | The timestamp of the date and time that the slot was created.
createSlotResponse_creationDateTime :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.UTCTime)
createSlotResponse_creationDateTime = Lens.lens (\CreateSlotResponse' {creationDateTime} -> creationDateTime) (\s@CreateSlotResponse' {} a -> s {creationDateTime = a} :: CreateSlotResponse) Prelude.. Lens.mapping Data._Time

-- | The description associated with the slot.
createSlotResponse_description :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_description = Lens.lens (\CreateSlotResponse' {description} -> description) (\s@CreateSlotResponse' {} a -> s {description = a} :: CreateSlotResponse)

-- | The unique identifier of the intent associated with the slot.
createSlotResponse_intentId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_intentId = Lens.lens (\CreateSlotResponse' {intentId} -> intentId) (\s@CreateSlotResponse' {} a -> s {intentId = a} :: CreateSlotResponse)

-- | The language and local specified for the slot.
createSlotResponse_localeId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_localeId = Lens.lens (\CreateSlotResponse' {localeId} -> localeId) (\s@CreateSlotResponse' {} a -> s {localeId = a} :: CreateSlotResponse)

-- | Indicates whether the slot returns multiple values in one response.
createSlotResponse_multipleValuesSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe MultipleValuesSetting)
createSlotResponse_multipleValuesSetting = Lens.lens (\CreateSlotResponse' {multipleValuesSetting} -> multipleValuesSetting) (\s@CreateSlotResponse' {} a -> s {multipleValuesSetting = a} :: CreateSlotResponse)

-- | Indicates whether the slot is configured to obfuscate values in Amazon
-- CloudWatch logs.
createSlotResponse_obfuscationSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe ObfuscationSetting)
createSlotResponse_obfuscationSetting = Lens.lens (\CreateSlotResponse' {obfuscationSetting} -> obfuscationSetting) (\s@CreateSlotResponse' {} a -> s {obfuscationSetting = a} :: CreateSlotResponse)

-- | The unique identifier associated with the slot. Use this to identify the
-- slot when you update or delete it.
createSlotResponse_slotId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotId = Lens.lens (\CreateSlotResponse' {slotId} -> slotId) (\s@CreateSlotResponse' {} a -> s {slotId = a} :: CreateSlotResponse)

-- | The name specified for the slot.
createSlotResponse_slotName :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotName = Lens.lens (\CreateSlotResponse' {slotName} -> slotName) (\s@CreateSlotResponse' {} a -> s {slotName = a} :: CreateSlotResponse)

-- | The unique identifier of the slot type associated with this slot.
createSlotResponse_slotTypeId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotTypeId = Lens.lens (\CreateSlotResponse' {slotTypeId} -> slotTypeId) (\s@CreateSlotResponse' {} a -> s {slotTypeId = a} :: CreateSlotResponse)

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
createSlotResponse_subSlotSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe SubSlotSetting)
createSlotResponse_subSlotSetting = Lens.lens (\CreateSlotResponse' {subSlotSetting} -> subSlotSetting) (\s@CreateSlotResponse' {} a -> s {subSlotSetting = a} :: CreateSlotResponse)

-- | The value elicitation settings specified for the slot.
createSlotResponse_valueElicitationSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe SlotValueElicitationSetting)
createSlotResponse_valueElicitationSetting = Lens.lens (\CreateSlotResponse' {valueElicitationSetting} -> valueElicitationSetting) (\s@CreateSlotResponse' {} a -> s {valueElicitationSetting = a} :: CreateSlotResponse)

-- | The response's http status code.
createSlotResponse_httpStatus :: Lens.Lens' CreateSlotResponse Prelude.Int
createSlotResponse_httpStatus = Lens.lens (\CreateSlotResponse' {httpStatus} -> httpStatus) (\s@CreateSlotResponse' {} a -> s {httpStatus = a} :: CreateSlotResponse)

instance Prelude.NFData CreateSlotResponse where
  rnf CreateSlotResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf multipleValuesSetting
      `Prelude.seq` Prelude.rnf obfuscationSetting
      `Prelude.seq` Prelude.rnf slotId
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf subSlotSetting
      `Prelude.seq` Prelude.rnf valueElicitationSetting
      `Prelude.seq` Prelude.rnf httpStatus
