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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createSlot_multipleValuesSetting,
    createSlot_description,
    createSlot_obfuscationSetting,
    createSlot_subSlotSetting,
    createSlot_slotTypeId,
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
    createSlotResponse_multipleValuesSetting,
    createSlotResponse_slotName,
    createSlotResponse_valueElicitationSetting,
    createSlotResponse_botVersion,
    createSlotResponse_creationDateTime,
    createSlotResponse_localeId,
    createSlotResponse_description,
    createSlotResponse_botId,
    createSlotResponse_intentId,
    createSlotResponse_slotId,
    createSlotResponse_obfuscationSetting,
    createSlotResponse_subSlotSetting,
    createSlotResponse_slotTypeId,
    createSlotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSlot' smart constructor.
data CreateSlot = CreateSlot'
  { -- | Indicates whether the slot returns multiple values in one response.
    -- Multi-value slots are only available in the en-US locale. If you set
    -- this value to @true@ in any other locale, Amazon Lex throws a
    -- @ValidationException@.
    --
    -- If the @multipleValuesSetting@ is not set, the default value is @false@.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | A description of the slot. Use this to help identify the slot in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | Determines how slot values are used in Amazon CloudWatch logs. If the
    -- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
    -- slot values are obfuscated in the log output. If the value is @None@,
    -- the actual value is present in the log output.
    --
    -- The default is to obfuscate values in the CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | Specifications for the constituent sub slots and the expression for the
    -- composite slot.
    subSlotSetting :: Prelude.Maybe SubSlotSetting,
    -- | The unique identifier for the slot type associated with this slot. The
    -- slot type determines the values that can be entered into the slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
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
-- 'multipleValuesSetting', 'createSlot_multipleValuesSetting' - Indicates whether the slot returns multiple values in one response.
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
--
-- 'description', 'createSlot_description' - A description of the slot. Use this to help identify the slot in lists.
--
-- 'obfuscationSetting', 'createSlot_obfuscationSetting' - Determines how slot values are used in Amazon CloudWatch logs. If the
-- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
-- slot values are obfuscated in the log output. If the value is @None@,
-- the actual value is present in the log output.
--
-- The default is to obfuscate values in the CloudWatch logs.
--
-- 'subSlotSetting', 'createSlot_subSlotSetting' - Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- 'slotTypeId', 'createSlot_slotTypeId' - The unique identifier for the slot type associated with this slot. The
-- slot type determines the values that can be entered into the slot.
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
      { multipleValuesSetting =
          Prelude.Nothing,
        description = Prelude.Nothing,
        obfuscationSetting = Prelude.Nothing,
        subSlotSetting = Prelude.Nothing,
        slotTypeId = Prelude.Nothing,
        slotName = pSlotName_,
        valueElicitationSetting = pValueElicitationSetting_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | Indicates whether the slot returns multiple values in one response.
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
createSlot_multipleValuesSetting :: Lens.Lens' CreateSlot (Prelude.Maybe MultipleValuesSetting)
createSlot_multipleValuesSetting = Lens.lens (\CreateSlot' {multipleValuesSetting} -> multipleValuesSetting) (\s@CreateSlot' {} a -> s {multipleValuesSetting = a} :: CreateSlot)

-- | A description of the slot. Use this to help identify the slot in lists.
createSlot_description :: Lens.Lens' CreateSlot (Prelude.Maybe Prelude.Text)
createSlot_description = Lens.lens (\CreateSlot' {description} -> description) (\s@CreateSlot' {} a -> s {description = a} :: CreateSlot)

-- | Determines how slot values are used in Amazon CloudWatch logs. If the
-- value of the @obfuscationSetting@ parameter is @DefaultObfuscation@,
-- slot values are obfuscated in the log output. If the value is @None@,
-- the actual value is present in the log output.
--
-- The default is to obfuscate values in the CloudWatch logs.
createSlot_obfuscationSetting :: Lens.Lens' CreateSlot (Prelude.Maybe ObfuscationSetting)
createSlot_obfuscationSetting = Lens.lens (\CreateSlot' {obfuscationSetting} -> obfuscationSetting) (\s@CreateSlot' {} a -> s {obfuscationSetting = a} :: CreateSlot)

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
createSlot_subSlotSetting :: Lens.Lens' CreateSlot (Prelude.Maybe SubSlotSetting)
createSlot_subSlotSetting = Lens.lens (\CreateSlot' {subSlotSetting} -> subSlotSetting) (\s@CreateSlot' {} a -> s {subSlotSetting = a} :: CreateSlot)

-- | The unique identifier for the slot type associated with this slot. The
-- slot type determines the values that can be entered into the slot.
createSlot_slotTypeId :: Lens.Lens' CreateSlot (Prelude.Maybe Prelude.Text)
createSlot_slotTypeId = Lens.lens (\CreateSlot' {slotTypeId} -> slotTypeId) (\s@CreateSlot' {} a -> s {slotTypeId = a} :: CreateSlot)

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
            Prelude.<$> (x Core..?> "multipleValuesSetting")
            Prelude.<*> (x Core..?> "slotName")
            Prelude.<*> (x Core..?> "valueElicitationSetting")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "intentId")
            Prelude.<*> (x Core..?> "slotId")
            Prelude.<*> (x Core..?> "obfuscationSetting")
            Prelude.<*> (x Core..?> "subSlotSetting")
            Prelude.<*> (x Core..?> "slotTypeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlot where
  hashWithSalt _salt CreateSlot' {..} =
    _salt `Prelude.hashWithSalt` multipleValuesSetting
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` obfuscationSetting
      `Prelude.hashWithSalt` subSlotSetting
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` valueElicitationSetting
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` intentId

instance Prelude.NFData CreateSlot where
  rnf CreateSlot' {..} =
    Prelude.rnf multipleValuesSetting
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf obfuscationSetting
      `Prelude.seq` Prelude.rnf subSlotSetting
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf valueElicitationSetting
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf intentId

instance Core.ToHeaders CreateSlot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSlot where
  toJSON CreateSlot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("multipleValuesSetting" Core..=)
              Prelude.<$> multipleValuesSetting,
            ("description" Core..=) Prelude.<$> description,
            ("obfuscationSetting" Core..=)
              Prelude.<$> obfuscationSetting,
            ("subSlotSetting" Core..=)
              Prelude.<$> subSlotSetting,
            ("slotTypeId" Core..=) Prelude.<$> slotTypeId,
            Prelude.Just ("slotName" Core..= slotName),
            Prelude.Just
              ( "valueElicitationSetting"
                  Core..= valueElicitationSetting
              )
          ]
      )

instance Core.ToPath CreateSlot where
  toPath CreateSlot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/",
        Core.toBS intentId,
        "/slots/"
      ]

instance Core.ToQuery CreateSlot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlotResponse' smart constructor.
data CreateSlotResponse = CreateSlotResponse'
  { -- | Indicates whether the slot returns multiple values in one response.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | The name specified for the slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The value elicitation settings specified for the slot.
    valueElicitationSetting :: Prelude.Maybe SlotValueElicitationSetting,
    -- | The version of the bot associated with the slot.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the date and time that the slot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The language and local specified for the slot.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The description associated with the slot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot associated with the slot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the intent associated with the slot.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier associated with the slot. Use this to identify the
    -- slot when you update or delete it.
    slotId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the slot is configured to obfuscate values in Amazon
    -- CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | Specifications for the constituent sub slots and the expression for the
    -- composite slot.
    subSlotSetting :: Prelude.Maybe SubSlotSetting,
    -- | The unique identifier of the slot type associated with this slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
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
-- 'multipleValuesSetting', 'createSlotResponse_multipleValuesSetting' - Indicates whether the slot returns multiple values in one response.
--
-- 'slotName', 'createSlotResponse_slotName' - The name specified for the slot.
--
-- 'valueElicitationSetting', 'createSlotResponse_valueElicitationSetting' - The value elicitation settings specified for the slot.
--
-- 'botVersion', 'createSlotResponse_botVersion' - The version of the bot associated with the slot.
--
-- 'creationDateTime', 'createSlotResponse_creationDateTime' - The timestamp of the date and time that the slot was created.
--
-- 'localeId', 'createSlotResponse_localeId' - The language and local specified for the slot.
--
-- 'description', 'createSlotResponse_description' - The description associated with the slot.
--
-- 'botId', 'createSlotResponse_botId' - The unique identifier of the bot associated with the slot.
--
-- 'intentId', 'createSlotResponse_intentId' - The unique identifier of the intent associated with the slot.
--
-- 'slotId', 'createSlotResponse_slotId' - The unique identifier associated with the slot. Use this to identify the
-- slot when you update or delete it.
--
-- 'obfuscationSetting', 'createSlotResponse_obfuscationSetting' - Indicates whether the slot is configured to obfuscate values in Amazon
-- CloudWatch logs.
--
-- 'subSlotSetting', 'createSlotResponse_subSlotSetting' - Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- 'slotTypeId', 'createSlotResponse_slotTypeId' - The unique identifier of the slot type associated with this slot.
--
-- 'httpStatus', 'createSlotResponse_httpStatus' - The response's http status code.
newCreateSlotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotResponse
newCreateSlotResponse pHttpStatus_ =
  CreateSlotResponse'
    { multipleValuesSetting =
        Prelude.Nothing,
      slotName = Prelude.Nothing,
      valueElicitationSetting = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      localeId = Prelude.Nothing,
      description = Prelude.Nothing,
      botId = Prelude.Nothing,
      intentId = Prelude.Nothing,
      slotId = Prelude.Nothing,
      obfuscationSetting = Prelude.Nothing,
      subSlotSetting = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the slot returns multiple values in one response.
createSlotResponse_multipleValuesSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe MultipleValuesSetting)
createSlotResponse_multipleValuesSetting = Lens.lens (\CreateSlotResponse' {multipleValuesSetting} -> multipleValuesSetting) (\s@CreateSlotResponse' {} a -> s {multipleValuesSetting = a} :: CreateSlotResponse)

-- | The name specified for the slot.
createSlotResponse_slotName :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotName = Lens.lens (\CreateSlotResponse' {slotName} -> slotName) (\s@CreateSlotResponse' {} a -> s {slotName = a} :: CreateSlotResponse)

-- | The value elicitation settings specified for the slot.
createSlotResponse_valueElicitationSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe SlotValueElicitationSetting)
createSlotResponse_valueElicitationSetting = Lens.lens (\CreateSlotResponse' {valueElicitationSetting} -> valueElicitationSetting) (\s@CreateSlotResponse' {} a -> s {valueElicitationSetting = a} :: CreateSlotResponse)

-- | The version of the bot associated with the slot.
createSlotResponse_botVersion :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_botVersion = Lens.lens (\CreateSlotResponse' {botVersion} -> botVersion) (\s@CreateSlotResponse' {} a -> s {botVersion = a} :: CreateSlotResponse)

-- | The timestamp of the date and time that the slot was created.
createSlotResponse_creationDateTime :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.UTCTime)
createSlotResponse_creationDateTime = Lens.lens (\CreateSlotResponse' {creationDateTime} -> creationDateTime) (\s@CreateSlotResponse' {} a -> s {creationDateTime = a} :: CreateSlotResponse) Prelude.. Lens.mapping Core._Time

-- | The language and local specified for the slot.
createSlotResponse_localeId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_localeId = Lens.lens (\CreateSlotResponse' {localeId} -> localeId) (\s@CreateSlotResponse' {} a -> s {localeId = a} :: CreateSlotResponse)

-- | The description associated with the slot.
createSlotResponse_description :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_description = Lens.lens (\CreateSlotResponse' {description} -> description) (\s@CreateSlotResponse' {} a -> s {description = a} :: CreateSlotResponse)

-- | The unique identifier of the bot associated with the slot.
createSlotResponse_botId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_botId = Lens.lens (\CreateSlotResponse' {botId} -> botId) (\s@CreateSlotResponse' {} a -> s {botId = a} :: CreateSlotResponse)

-- | The unique identifier of the intent associated with the slot.
createSlotResponse_intentId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_intentId = Lens.lens (\CreateSlotResponse' {intentId} -> intentId) (\s@CreateSlotResponse' {} a -> s {intentId = a} :: CreateSlotResponse)

-- | The unique identifier associated with the slot. Use this to identify the
-- slot when you update or delete it.
createSlotResponse_slotId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotId = Lens.lens (\CreateSlotResponse' {slotId} -> slotId) (\s@CreateSlotResponse' {} a -> s {slotId = a} :: CreateSlotResponse)

-- | Indicates whether the slot is configured to obfuscate values in Amazon
-- CloudWatch logs.
createSlotResponse_obfuscationSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe ObfuscationSetting)
createSlotResponse_obfuscationSetting = Lens.lens (\CreateSlotResponse' {obfuscationSetting} -> obfuscationSetting) (\s@CreateSlotResponse' {} a -> s {obfuscationSetting = a} :: CreateSlotResponse)

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
createSlotResponse_subSlotSetting :: Lens.Lens' CreateSlotResponse (Prelude.Maybe SubSlotSetting)
createSlotResponse_subSlotSetting = Lens.lens (\CreateSlotResponse' {subSlotSetting} -> subSlotSetting) (\s@CreateSlotResponse' {} a -> s {subSlotSetting = a} :: CreateSlotResponse)

-- | The unique identifier of the slot type associated with this slot.
createSlotResponse_slotTypeId :: Lens.Lens' CreateSlotResponse (Prelude.Maybe Prelude.Text)
createSlotResponse_slotTypeId = Lens.lens (\CreateSlotResponse' {slotTypeId} -> slotTypeId) (\s@CreateSlotResponse' {} a -> s {slotTypeId = a} :: CreateSlotResponse)

-- | The response's http status code.
createSlotResponse_httpStatus :: Lens.Lens' CreateSlotResponse Prelude.Int
createSlotResponse_httpStatus = Lens.lens (\CreateSlotResponse' {httpStatus} -> httpStatus) (\s@CreateSlotResponse' {} a -> s {httpStatus = a} :: CreateSlotResponse)

instance Prelude.NFData CreateSlotResponse where
  rnf CreateSlotResponse' {..} =
    Prelude.rnf multipleValuesSetting
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf valueElicitationSetting
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf slotId
      `Prelude.seq` Prelude.rnf obfuscationSetting
      `Prelude.seq` Prelude.rnf subSlotSetting
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf httpStatus
