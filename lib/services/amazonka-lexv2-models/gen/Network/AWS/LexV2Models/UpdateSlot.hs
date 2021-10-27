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
-- Module      : Network.AWS.LexV2Models.UpdateSlot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for a slot.
module Network.AWS.LexV2Models.UpdateSlot
  ( -- * Creating a Request
    UpdateSlot (..),
    newUpdateSlot,

    -- * Request Lenses
    updateSlot_obfuscationSetting,
    updateSlot_multipleValuesSetting,
    updateSlot_description,
    updateSlot_slotId,
    updateSlot_slotName,
    updateSlot_slotTypeId,
    updateSlot_valueElicitationSetting,
    updateSlot_botId,
    updateSlot_botVersion,
    updateSlot_localeId,
    updateSlot_intentId,

    -- * Destructuring the Response
    UpdateSlotResponse (..),
    newUpdateSlotResponse,

    -- * Response Lenses
    updateSlotResponse_obfuscationSetting,
    updateSlotResponse_slotName,
    updateSlotResponse_botVersion,
    updateSlotResponse_valueElicitationSetting,
    updateSlotResponse_lastUpdatedDateTime,
    updateSlotResponse_multipleValuesSetting,
    updateSlotResponse_slotId,
    updateSlotResponse_intentId,
    updateSlotResponse_botId,
    updateSlotResponse_localeId,
    updateSlotResponse_creationDateTime,
    updateSlotResponse_description,
    updateSlotResponse_slotTypeId,
    updateSlotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSlot' smart constructor.
data UpdateSlot = UpdateSlot'
  { -- | New settings that determine how slot values are formatted in Amazon
    -- CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | Determines whether the slot accepts multiple values in one response.
    -- Multiple value slots are only available in the en-US locale. If you set
    -- this value to @true@ in any other locale, Amazon Lex throws a
    -- @ValidationException@.
    --
    -- If the @multipleValuesSetting@ is not set, the default value is @false@.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | The new description for the slot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the slot to update.
    slotId :: Prelude.Text,
    -- | The new name for the slot.
    slotName :: Prelude.Text,
    -- | The unique identifier of the new slot type to associate with this slot.
    slotTypeId :: Prelude.Text,
    -- | A new set of prompts that Amazon Lex sends to the user to elicit a
    -- response the provides a value for the slot.
    valueElicitationSetting :: SlotValueElicitationSetting,
    -- | The unique identifier of the bot that contains the slot.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the slot. Must always be @DRAFT@.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that contains the slot. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The identifier of the intent that contains the slot.
    intentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSlot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'obfuscationSetting', 'updateSlot_obfuscationSetting' - New settings that determine how slot values are formatted in Amazon
-- CloudWatch logs.
--
-- 'multipleValuesSetting', 'updateSlot_multipleValuesSetting' - Determines whether the slot accepts multiple values in one response.
-- Multiple value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
--
-- 'description', 'updateSlot_description' - The new description for the slot.
--
-- 'slotId', 'updateSlot_slotId' - The unique identifier for the slot to update.
--
-- 'slotName', 'updateSlot_slotName' - The new name for the slot.
--
-- 'slotTypeId', 'updateSlot_slotTypeId' - The unique identifier of the new slot type to associate with this slot.
--
-- 'valueElicitationSetting', 'updateSlot_valueElicitationSetting' - A new set of prompts that Amazon Lex sends to the user to elicit a
-- response the provides a value for the slot.
--
-- 'botId', 'updateSlot_botId' - The unique identifier of the bot that contains the slot.
--
-- 'botVersion', 'updateSlot_botVersion' - The version of the bot that contains the slot. Must always be @DRAFT@.
--
-- 'localeId', 'updateSlot_localeId' - The identifier of the language and locale that contains the slot. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'intentId', 'updateSlot_intentId' - The identifier of the intent that contains the slot.
newUpdateSlot ::
  -- | 'slotId'
  Prelude.Text ->
  -- | 'slotName'
  Prelude.Text ->
  -- | 'slotTypeId'
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
  UpdateSlot
newUpdateSlot
  pSlotId_
  pSlotName_
  pSlotTypeId_
  pValueElicitationSetting_
  pBotId_
  pBotVersion_
  pLocaleId_
  pIntentId_ =
    UpdateSlot'
      { obfuscationSetting = Prelude.Nothing,
        multipleValuesSetting = Prelude.Nothing,
        description = Prelude.Nothing,
        slotId = pSlotId_,
        slotName = pSlotName_,
        slotTypeId = pSlotTypeId_,
        valueElicitationSetting = pValueElicitationSetting_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | New settings that determine how slot values are formatted in Amazon
-- CloudWatch logs.
updateSlot_obfuscationSetting :: Lens.Lens' UpdateSlot (Prelude.Maybe ObfuscationSetting)
updateSlot_obfuscationSetting = Lens.lens (\UpdateSlot' {obfuscationSetting} -> obfuscationSetting) (\s@UpdateSlot' {} a -> s {obfuscationSetting = a} :: UpdateSlot)

-- | Determines whether the slot accepts multiple values in one response.
-- Multiple value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
updateSlot_multipleValuesSetting :: Lens.Lens' UpdateSlot (Prelude.Maybe MultipleValuesSetting)
updateSlot_multipleValuesSetting = Lens.lens (\UpdateSlot' {multipleValuesSetting} -> multipleValuesSetting) (\s@UpdateSlot' {} a -> s {multipleValuesSetting = a} :: UpdateSlot)

-- | The new description for the slot.
updateSlot_description :: Lens.Lens' UpdateSlot (Prelude.Maybe Prelude.Text)
updateSlot_description = Lens.lens (\UpdateSlot' {description} -> description) (\s@UpdateSlot' {} a -> s {description = a} :: UpdateSlot)

-- | The unique identifier for the slot to update.
updateSlot_slotId :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_slotId = Lens.lens (\UpdateSlot' {slotId} -> slotId) (\s@UpdateSlot' {} a -> s {slotId = a} :: UpdateSlot)

-- | The new name for the slot.
updateSlot_slotName :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_slotName = Lens.lens (\UpdateSlot' {slotName} -> slotName) (\s@UpdateSlot' {} a -> s {slotName = a} :: UpdateSlot)

-- | The unique identifier of the new slot type to associate with this slot.
updateSlot_slotTypeId :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_slotTypeId = Lens.lens (\UpdateSlot' {slotTypeId} -> slotTypeId) (\s@UpdateSlot' {} a -> s {slotTypeId = a} :: UpdateSlot)

-- | A new set of prompts that Amazon Lex sends to the user to elicit a
-- response the provides a value for the slot.
updateSlot_valueElicitationSetting :: Lens.Lens' UpdateSlot SlotValueElicitationSetting
updateSlot_valueElicitationSetting = Lens.lens (\UpdateSlot' {valueElicitationSetting} -> valueElicitationSetting) (\s@UpdateSlot' {} a -> s {valueElicitationSetting = a} :: UpdateSlot)

-- | The unique identifier of the bot that contains the slot.
updateSlot_botId :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_botId = Lens.lens (\UpdateSlot' {botId} -> botId) (\s@UpdateSlot' {} a -> s {botId = a} :: UpdateSlot)

-- | The version of the bot that contains the slot. Must always be @DRAFT@.
updateSlot_botVersion :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_botVersion = Lens.lens (\UpdateSlot' {botVersion} -> botVersion) (\s@UpdateSlot' {} a -> s {botVersion = a} :: UpdateSlot)

-- | The identifier of the language and locale that contains the slot. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
updateSlot_localeId :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_localeId = Lens.lens (\UpdateSlot' {localeId} -> localeId) (\s@UpdateSlot' {} a -> s {localeId = a} :: UpdateSlot)

-- | The identifier of the intent that contains the slot.
updateSlot_intentId :: Lens.Lens' UpdateSlot Prelude.Text
updateSlot_intentId = Lens.lens (\UpdateSlot' {intentId} -> intentId) (\s@UpdateSlot' {} a -> s {intentId = a} :: UpdateSlot)

instance Core.AWSRequest UpdateSlot where
  type AWSResponse UpdateSlot = UpdateSlotResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSlotResponse'
            Prelude.<$> (x Core..?> "obfuscationSetting")
            Prelude.<*> (x Core..?> "slotName")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "valueElicitationSetting")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "multipleValuesSetting")
            Prelude.<*> (x Core..?> "slotId")
            Prelude.<*> (x Core..?> "intentId")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "slotTypeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSlot

instance Prelude.NFData UpdateSlot

instance Core.ToHeaders UpdateSlot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSlot where
  toJSON UpdateSlot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("obfuscationSetting" Core..=)
              Prelude.<$> obfuscationSetting,
            ("multipleValuesSetting" Core..=)
              Prelude.<$> multipleValuesSetting,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("slotName" Core..= slotName),
            Prelude.Just ("slotTypeId" Core..= slotTypeId),
            Prelude.Just
              ( "valueElicitationSetting"
                  Core..= valueElicitationSetting
              )
          ]
      )

instance Core.ToPath UpdateSlot where
  toPath UpdateSlot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/",
        Core.toBS intentId,
        "/slots/",
        Core.toBS slotId,
        "/"
      ]

instance Core.ToQuery UpdateSlot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSlotResponse' smart constructor.
data UpdateSlotResponse = UpdateSlotResponse'
  { -- | The updated setting that determines whether the slot value is obfuscated
    -- in the Amazon CloudWatch logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | The updated name of the slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the slot version that contains the slot. Will always
    -- be @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The updated prompts that Amazon Lex sends to the user to elicit a
    -- response that provides a value for the slot.
    valueElicitationSetting :: Prelude.Maybe SlotValueElicitationSetting,
    -- | The timestamp of the date and time that the slot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the slot accepts multiple values in one response.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | The unique identifier of the slot that was updated.
    slotId :: Prelude.Maybe Prelude.Text,
    -- | The intent that contains the slot.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the slot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The locale that contains the slot.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the date and time that the slot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The updated description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated identifier of the slot type that provides values for the
    -- slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSlotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'obfuscationSetting', 'updateSlotResponse_obfuscationSetting' - The updated setting that determines whether the slot value is obfuscated
-- in the Amazon CloudWatch logs.
--
-- 'slotName', 'updateSlotResponse_slotName' - The updated name of the slot.
--
-- 'botVersion', 'updateSlotResponse_botVersion' - The identifier of the slot version that contains the slot. Will always
-- be @DRAFT@.
--
-- 'valueElicitationSetting', 'updateSlotResponse_valueElicitationSetting' - The updated prompts that Amazon Lex sends to the user to elicit a
-- response that provides a value for the slot.
--
-- 'lastUpdatedDateTime', 'updateSlotResponse_lastUpdatedDateTime' - The timestamp of the date and time that the slot was last updated.
--
-- 'multipleValuesSetting', 'updateSlotResponse_multipleValuesSetting' - Indicates whether the slot accepts multiple values in one response.
--
-- 'slotId', 'updateSlotResponse_slotId' - The unique identifier of the slot that was updated.
--
-- 'intentId', 'updateSlotResponse_intentId' - The intent that contains the slot.
--
-- 'botId', 'updateSlotResponse_botId' - The identifier of the bot that contains the slot.
--
-- 'localeId', 'updateSlotResponse_localeId' - The locale that contains the slot.
--
-- 'creationDateTime', 'updateSlotResponse_creationDateTime' - The timestamp of the date and time that the slot was created.
--
-- 'description', 'updateSlotResponse_description' - The updated description of the bot.
--
-- 'slotTypeId', 'updateSlotResponse_slotTypeId' - The updated identifier of the slot type that provides values for the
-- slot.
--
-- 'httpStatus', 'updateSlotResponse_httpStatus' - The response's http status code.
newUpdateSlotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSlotResponse
newUpdateSlotResponse pHttpStatus_ =
  UpdateSlotResponse'
    { obfuscationSetting =
        Prelude.Nothing,
      slotName = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      valueElicitationSetting = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      multipleValuesSetting = Prelude.Nothing,
      slotId = Prelude.Nothing,
      intentId = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated setting that determines whether the slot value is obfuscated
-- in the Amazon CloudWatch logs.
updateSlotResponse_obfuscationSetting :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe ObfuscationSetting)
updateSlotResponse_obfuscationSetting = Lens.lens (\UpdateSlotResponse' {obfuscationSetting} -> obfuscationSetting) (\s@UpdateSlotResponse' {} a -> s {obfuscationSetting = a} :: UpdateSlotResponse)

-- | The updated name of the slot.
updateSlotResponse_slotName :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_slotName = Lens.lens (\UpdateSlotResponse' {slotName} -> slotName) (\s@UpdateSlotResponse' {} a -> s {slotName = a} :: UpdateSlotResponse)

-- | The identifier of the slot version that contains the slot. Will always
-- be @DRAFT@.
updateSlotResponse_botVersion :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_botVersion = Lens.lens (\UpdateSlotResponse' {botVersion} -> botVersion) (\s@UpdateSlotResponse' {} a -> s {botVersion = a} :: UpdateSlotResponse)

-- | The updated prompts that Amazon Lex sends to the user to elicit a
-- response that provides a value for the slot.
updateSlotResponse_valueElicitationSetting :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe SlotValueElicitationSetting)
updateSlotResponse_valueElicitationSetting = Lens.lens (\UpdateSlotResponse' {valueElicitationSetting} -> valueElicitationSetting) (\s@UpdateSlotResponse' {} a -> s {valueElicitationSetting = a} :: UpdateSlotResponse)

-- | The timestamp of the date and time that the slot was last updated.
updateSlotResponse_lastUpdatedDateTime :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotResponse_lastUpdatedDateTime = Lens.lens (\UpdateSlotResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateSlotResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateSlotResponse) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the slot accepts multiple values in one response.
updateSlotResponse_multipleValuesSetting :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe MultipleValuesSetting)
updateSlotResponse_multipleValuesSetting = Lens.lens (\UpdateSlotResponse' {multipleValuesSetting} -> multipleValuesSetting) (\s@UpdateSlotResponse' {} a -> s {multipleValuesSetting = a} :: UpdateSlotResponse)

-- | The unique identifier of the slot that was updated.
updateSlotResponse_slotId :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_slotId = Lens.lens (\UpdateSlotResponse' {slotId} -> slotId) (\s@UpdateSlotResponse' {} a -> s {slotId = a} :: UpdateSlotResponse)

-- | The intent that contains the slot.
updateSlotResponse_intentId :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_intentId = Lens.lens (\UpdateSlotResponse' {intentId} -> intentId) (\s@UpdateSlotResponse' {} a -> s {intentId = a} :: UpdateSlotResponse)

-- | The identifier of the bot that contains the slot.
updateSlotResponse_botId :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_botId = Lens.lens (\UpdateSlotResponse' {botId} -> botId) (\s@UpdateSlotResponse' {} a -> s {botId = a} :: UpdateSlotResponse)

-- | The locale that contains the slot.
updateSlotResponse_localeId :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_localeId = Lens.lens (\UpdateSlotResponse' {localeId} -> localeId) (\s@UpdateSlotResponse' {} a -> s {localeId = a} :: UpdateSlotResponse)

-- | The timestamp of the date and time that the slot was created.
updateSlotResponse_creationDateTime :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.UTCTime)
updateSlotResponse_creationDateTime = Lens.lens (\UpdateSlotResponse' {creationDateTime} -> creationDateTime) (\s@UpdateSlotResponse' {} a -> s {creationDateTime = a} :: UpdateSlotResponse) Prelude.. Lens.mapping Core._Time

-- | The updated description of the bot.
updateSlotResponse_description :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_description = Lens.lens (\UpdateSlotResponse' {description} -> description) (\s@UpdateSlotResponse' {} a -> s {description = a} :: UpdateSlotResponse)

-- | The updated identifier of the slot type that provides values for the
-- slot.
updateSlotResponse_slotTypeId :: Lens.Lens' UpdateSlotResponse (Prelude.Maybe Prelude.Text)
updateSlotResponse_slotTypeId = Lens.lens (\UpdateSlotResponse' {slotTypeId} -> slotTypeId) (\s@UpdateSlotResponse' {} a -> s {slotTypeId = a} :: UpdateSlotResponse)

-- | The response's http status code.
updateSlotResponse_httpStatus :: Lens.Lens' UpdateSlotResponse Prelude.Int
updateSlotResponse_httpStatus = Lens.lens (\UpdateSlotResponse' {httpStatus} -> httpStatus) (\s@UpdateSlotResponse' {} a -> s {httpStatus = a} :: UpdateSlotResponse)

instance Prelude.NFData UpdateSlotResponse
