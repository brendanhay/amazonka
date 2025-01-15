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
-- Module      : Amazonka.LexV2Models.DescribeSlot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about a slot.
module Amazonka.LexV2Models.DescribeSlot
  ( -- * Creating a Request
    DescribeSlot (..),
    newDescribeSlot,

    -- * Request Lenses
    describeSlot_slotId,
    describeSlot_botId,
    describeSlot_botVersion,
    describeSlot_localeId,
    describeSlot_intentId,

    -- * Destructuring the Response
    DescribeSlotResponse (..),
    newDescribeSlotResponse,

    -- * Response Lenses
    describeSlotResponse_botId,
    describeSlotResponse_botVersion,
    describeSlotResponse_creationDateTime,
    describeSlotResponse_description,
    describeSlotResponse_intentId,
    describeSlotResponse_lastUpdatedDateTime,
    describeSlotResponse_localeId,
    describeSlotResponse_multipleValuesSetting,
    describeSlotResponse_obfuscationSetting,
    describeSlotResponse_slotId,
    describeSlotResponse_slotName,
    describeSlotResponse_slotTypeId,
    describeSlotResponse_subSlotSetting,
    describeSlotResponse_valueElicitationSetting,
    describeSlotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSlot' smart constructor.
data DescribeSlot = DescribeSlot'
  { -- | The unique identifier for the slot.
    slotId :: Prelude.Text,
    -- | The identifier of the bot associated with the slot.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the slot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the slot to describe. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The identifier of the intent that contains the slot.
    intentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSlot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotId', 'describeSlot_slotId' - The unique identifier for the slot.
--
-- 'botId', 'describeSlot_botId' - The identifier of the bot associated with the slot.
--
-- 'botVersion', 'describeSlot_botVersion' - The version of the bot associated with the slot.
--
-- 'localeId', 'describeSlot_localeId' - The identifier of the language and locale of the slot to describe. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'intentId', 'describeSlot_intentId' - The identifier of the intent that contains the slot.
newDescribeSlot ::
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
  DescribeSlot
newDescribeSlot
  pSlotId_
  pBotId_
  pBotVersion_
  pLocaleId_
  pIntentId_ =
    DescribeSlot'
      { slotId = pSlotId_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | The unique identifier for the slot.
describeSlot_slotId :: Lens.Lens' DescribeSlot Prelude.Text
describeSlot_slotId = Lens.lens (\DescribeSlot' {slotId} -> slotId) (\s@DescribeSlot' {} a -> s {slotId = a} :: DescribeSlot)

-- | The identifier of the bot associated with the slot.
describeSlot_botId :: Lens.Lens' DescribeSlot Prelude.Text
describeSlot_botId = Lens.lens (\DescribeSlot' {botId} -> botId) (\s@DescribeSlot' {} a -> s {botId = a} :: DescribeSlot)

-- | The version of the bot associated with the slot.
describeSlot_botVersion :: Lens.Lens' DescribeSlot Prelude.Text
describeSlot_botVersion = Lens.lens (\DescribeSlot' {botVersion} -> botVersion) (\s@DescribeSlot' {} a -> s {botVersion = a} :: DescribeSlot)

-- | The identifier of the language and locale of the slot to describe. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
describeSlot_localeId :: Lens.Lens' DescribeSlot Prelude.Text
describeSlot_localeId = Lens.lens (\DescribeSlot' {localeId} -> localeId) (\s@DescribeSlot' {} a -> s {localeId = a} :: DescribeSlot)

-- | The identifier of the intent that contains the slot.
describeSlot_intentId :: Lens.Lens' DescribeSlot Prelude.Text
describeSlot_intentId = Lens.lens (\DescribeSlot' {intentId} -> intentId) (\s@DescribeSlot' {} a -> s {intentId = a} :: DescribeSlot)

instance Core.AWSRequest DescribeSlot where
  type AWSResponse DescribeSlot = DescribeSlotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSlotResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "intentId")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
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

instance Prelude.Hashable DescribeSlot where
  hashWithSalt _salt DescribeSlot' {..} =
    _salt
      `Prelude.hashWithSalt` slotId
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` intentId

instance Prelude.NFData DescribeSlot where
  rnf DescribeSlot' {..} =
    Prelude.rnf slotId `Prelude.seq`
      Prelude.rnf botId `Prelude.seq`
        Prelude.rnf botVersion `Prelude.seq`
          Prelude.rnf localeId `Prelude.seq`
            Prelude.rnf intentId

instance Data.ToHeaders DescribeSlot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSlot where
  toPath DescribeSlot' {..} =
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

instance Data.ToQuery DescribeSlot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSlotResponse' smart constructor.
data DescribeSlotResponse = DescribeSlotResponse'
  { -- | The identifier of the bot associated with the slot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot associated with the slot.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description specified for the slot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the intent associated with the slot.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The language and locale specified for the slot.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the slot accepts multiple values in a single
    -- utterance.
    --
    -- If the @multipleValuesSetting@ is not set, the default value is @false@.
    multipleValuesSetting :: Prelude.Maybe MultipleValuesSetting,
    -- | Whether slot values are shown in Amazon CloudWatch logs. If the value is
    -- @None@, the actual value of the slot is shown in logs.
    obfuscationSetting :: Prelude.Maybe ObfuscationSetting,
    -- | The unique identifier generated for the slot.
    slotId :: Prelude.Maybe Prelude.Text,
    -- | The name specified for the slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the slot type that determines the values entered into
    -- the slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | Specifications for the constituent sub slots and the expression for the
    -- composite slot.
    subSlotSetting :: Prelude.Maybe SubSlotSetting,
    -- | Prompts that Amazon Lex uses to elicit a value for the slot.
    valueElicitationSetting :: Prelude.Maybe SlotValueElicitationSetting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSlotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeSlotResponse_botId' - The identifier of the bot associated with the slot.
--
-- 'botVersion', 'describeSlotResponse_botVersion' - The version of the bot associated with the slot.
--
-- 'creationDateTime', 'describeSlotResponse_creationDateTime' - A timestamp of the date and time that the slot was created.
--
-- 'description', 'describeSlotResponse_description' - The description specified for the slot.
--
-- 'intentId', 'describeSlotResponse_intentId' - The identifier of the intent associated with the slot.
--
-- 'lastUpdatedDateTime', 'describeSlotResponse_lastUpdatedDateTime' - A timestamp of the date and time that the slot was last updated.
--
-- 'localeId', 'describeSlotResponse_localeId' - The language and locale specified for the slot.
--
-- 'multipleValuesSetting', 'describeSlotResponse_multipleValuesSetting' - Indicates whether the slot accepts multiple values in a single
-- utterance.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
--
-- 'obfuscationSetting', 'describeSlotResponse_obfuscationSetting' - Whether slot values are shown in Amazon CloudWatch logs. If the value is
-- @None@, the actual value of the slot is shown in logs.
--
-- 'slotId', 'describeSlotResponse_slotId' - The unique identifier generated for the slot.
--
-- 'slotName', 'describeSlotResponse_slotName' - The name specified for the slot.
--
-- 'slotTypeId', 'describeSlotResponse_slotTypeId' - The identifier of the slot type that determines the values entered into
-- the slot.
--
-- 'subSlotSetting', 'describeSlotResponse_subSlotSetting' - Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- 'valueElicitationSetting', 'describeSlotResponse_valueElicitationSetting' - Prompts that Amazon Lex uses to elicit a value for the slot.
--
-- 'httpStatus', 'describeSlotResponse_httpStatus' - The response's http status code.
newDescribeSlotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSlotResponse
newDescribeSlotResponse pHttpStatus_ =
  DescribeSlotResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      intentId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
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

-- | The identifier of the bot associated with the slot.
describeSlotResponse_botId :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_botId = Lens.lens (\DescribeSlotResponse' {botId} -> botId) (\s@DescribeSlotResponse' {} a -> s {botId = a} :: DescribeSlotResponse)

-- | The version of the bot associated with the slot.
describeSlotResponse_botVersion :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_botVersion = Lens.lens (\DescribeSlotResponse' {botVersion} -> botVersion) (\s@DescribeSlotResponse' {} a -> s {botVersion = a} :: DescribeSlotResponse)

-- | A timestamp of the date and time that the slot was created.
describeSlotResponse_creationDateTime :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotResponse_creationDateTime = Lens.lens (\DescribeSlotResponse' {creationDateTime} -> creationDateTime) (\s@DescribeSlotResponse' {} a -> s {creationDateTime = a} :: DescribeSlotResponse) Prelude.. Lens.mapping Data._Time

-- | The description specified for the slot.
describeSlotResponse_description :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_description = Lens.lens (\DescribeSlotResponse' {description} -> description) (\s@DescribeSlotResponse' {} a -> s {description = a} :: DescribeSlotResponse)

-- | The identifier of the intent associated with the slot.
describeSlotResponse_intentId :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_intentId = Lens.lens (\DescribeSlotResponse' {intentId} -> intentId) (\s@DescribeSlotResponse' {} a -> s {intentId = a} :: DescribeSlotResponse)

-- | A timestamp of the date and time that the slot was last updated.
describeSlotResponse_lastUpdatedDateTime :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotResponse_lastUpdatedDateTime = Lens.lens (\DescribeSlotResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeSlotResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeSlotResponse) Prelude.. Lens.mapping Data._Time

-- | The language and locale specified for the slot.
describeSlotResponse_localeId :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_localeId = Lens.lens (\DescribeSlotResponse' {localeId} -> localeId) (\s@DescribeSlotResponse' {} a -> s {localeId = a} :: DescribeSlotResponse)

-- | Indicates whether the slot accepts multiple values in a single
-- utterance.
--
-- If the @multipleValuesSetting@ is not set, the default value is @false@.
describeSlotResponse_multipleValuesSetting :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe MultipleValuesSetting)
describeSlotResponse_multipleValuesSetting = Lens.lens (\DescribeSlotResponse' {multipleValuesSetting} -> multipleValuesSetting) (\s@DescribeSlotResponse' {} a -> s {multipleValuesSetting = a} :: DescribeSlotResponse)

-- | Whether slot values are shown in Amazon CloudWatch logs. If the value is
-- @None@, the actual value of the slot is shown in logs.
describeSlotResponse_obfuscationSetting :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe ObfuscationSetting)
describeSlotResponse_obfuscationSetting = Lens.lens (\DescribeSlotResponse' {obfuscationSetting} -> obfuscationSetting) (\s@DescribeSlotResponse' {} a -> s {obfuscationSetting = a} :: DescribeSlotResponse)

-- | The unique identifier generated for the slot.
describeSlotResponse_slotId :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_slotId = Lens.lens (\DescribeSlotResponse' {slotId} -> slotId) (\s@DescribeSlotResponse' {} a -> s {slotId = a} :: DescribeSlotResponse)

-- | The name specified for the slot.
describeSlotResponse_slotName :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_slotName = Lens.lens (\DescribeSlotResponse' {slotName} -> slotName) (\s@DescribeSlotResponse' {} a -> s {slotName = a} :: DescribeSlotResponse)

-- | The identifier of the slot type that determines the values entered into
-- the slot.
describeSlotResponse_slotTypeId :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe Prelude.Text)
describeSlotResponse_slotTypeId = Lens.lens (\DescribeSlotResponse' {slotTypeId} -> slotTypeId) (\s@DescribeSlotResponse' {} a -> s {slotTypeId = a} :: DescribeSlotResponse)

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
describeSlotResponse_subSlotSetting :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe SubSlotSetting)
describeSlotResponse_subSlotSetting = Lens.lens (\DescribeSlotResponse' {subSlotSetting} -> subSlotSetting) (\s@DescribeSlotResponse' {} a -> s {subSlotSetting = a} :: DescribeSlotResponse)

-- | Prompts that Amazon Lex uses to elicit a value for the slot.
describeSlotResponse_valueElicitationSetting :: Lens.Lens' DescribeSlotResponse (Prelude.Maybe SlotValueElicitationSetting)
describeSlotResponse_valueElicitationSetting = Lens.lens (\DescribeSlotResponse' {valueElicitationSetting} -> valueElicitationSetting) (\s@DescribeSlotResponse' {} a -> s {valueElicitationSetting = a} :: DescribeSlotResponse)

-- | The response's http status code.
describeSlotResponse_httpStatus :: Lens.Lens' DescribeSlotResponse Prelude.Int
describeSlotResponse_httpStatus = Lens.lens (\DescribeSlotResponse' {httpStatus} -> httpStatus) (\s@DescribeSlotResponse' {} a -> s {httpStatus = a} :: DescribeSlotResponse)

instance Prelude.NFData DescribeSlotResponse where
  rnf DescribeSlotResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botVersion `Prelude.seq`
        Prelude.rnf creationDateTime `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf intentId `Prelude.seq`
              Prelude.rnf lastUpdatedDateTime `Prelude.seq`
                Prelude.rnf localeId `Prelude.seq`
                  Prelude.rnf multipleValuesSetting `Prelude.seq`
                    Prelude.rnf obfuscationSetting `Prelude.seq`
                      Prelude.rnf slotId `Prelude.seq`
                        Prelude.rnf slotName `Prelude.seq`
                          Prelude.rnf slotTypeId `Prelude.seq`
                            Prelude.rnf subSlotSetting `Prelude.seq`
                              Prelude.rnf valueElicitationSetting `Prelude.seq`
                                Prelude.rnf httpStatus
