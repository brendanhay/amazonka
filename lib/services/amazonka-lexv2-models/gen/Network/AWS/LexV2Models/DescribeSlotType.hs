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
-- Module      : Network.AWS.LexV2Models.DescribeSlotType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about a slot type.
module Network.AWS.LexV2Models.DescribeSlotType
  ( -- * Creating a Request
    DescribeSlotType (..),
    newDescribeSlotType,

    -- * Request Lenses
    describeSlotType_slotTypeId,
    describeSlotType_botId,
    describeSlotType_botVersion,
    describeSlotType_localeId,

    -- * Destructuring the Response
    DescribeSlotTypeResponse (..),
    newDescribeSlotTypeResponse,

    -- * Response Lenses
    describeSlotTypeResponse_parentSlotTypeSignature,
    describeSlotTypeResponse_slotTypeValues,
    describeSlotTypeResponse_valueSelectionSetting,
    describeSlotTypeResponse_botVersion,
    describeSlotTypeResponse_lastUpdatedDateTime,
    describeSlotTypeResponse_botId,
    describeSlotTypeResponse_localeId,
    describeSlotTypeResponse_creationDateTime,
    describeSlotTypeResponse_slotTypeName,
    describeSlotTypeResponse_description,
    describeSlotTypeResponse_slotTypeId,
    describeSlotTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSlotType' smart constructor.
data DescribeSlotType = DescribeSlotType'
  { -- | The identifier of the slot type.
    slotTypeId :: Prelude.Text,
    -- | The identifier of the bot associated with the slot type.
    botId :: Prelude.Text,
    -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the slot type to describe.
    -- The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotTypeId', 'describeSlotType_slotTypeId' - The identifier of the slot type.
--
-- 'botId', 'describeSlotType_botId' - The identifier of the bot associated with the slot type.
--
-- 'botVersion', 'describeSlotType_botVersion' - The version of the bot associated with the slot type.
--
-- 'localeId', 'describeSlotType_localeId' - The identifier of the language and locale of the slot type to describe.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newDescribeSlotType ::
  -- | 'slotTypeId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DescribeSlotType
newDescribeSlotType
  pSlotTypeId_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    DescribeSlotType'
      { slotTypeId = pSlotTypeId_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The identifier of the slot type.
describeSlotType_slotTypeId :: Lens.Lens' DescribeSlotType Prelude.Text
describeSlotType_slotTypeId = Lens.lens (\DescribeSlotType' {slotTypeId} -> slotTypeId) (\s@DescribeSlotType' {} a -> s {slotTypeId = a} :: DescribeSlotType)

-- | The identifier of the bot associated with the slot type.
describeSlotType_botId :: Lens.Lens' DescribeSlotType Prelude.Text
describeSlotType_botId = Lens.lens (\DescribeSlotType' {botId} -> botId) (\s@DescribeSlotType' {} a -> s {botId = a} :: DescribeSlotType)

-- | The version of the bot associated with the slot type.
describeSlotType_botVersion :: Lens.Lens' DescribeSlotType Prelude.Text
describeSlotType_botVersion = Lens.lens (\DescribeSlotType' {botVersion} -> botVersion) (\s@DescribeSlotType' {} a -> s {botVersion = a} :: DescribeSlotType)

-- | The identifier of the language and locale of the slot type to describe.
-- The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
describeSlotType_localeId :: Lens.Lens' DescribeSlotType Prelude.Text
describeSlotType_localeId = Lens.lens (\DescribeSlotType' {localeId} -> localeId) (\s@DescribeSlotType' {} a -> s {localeId = a} :: DescribeSlotType)

instance Core.AWSRequest DescribeSlotType where
  type
    AWSResponse DescribeSlotType =
      DescribeSlotTypeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSlotTypeResponse'
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

instance Prelude.Hashable DescribeSlotType

instance Prelude.NFData DescribeSlotType

instance Core.ToHeaders DescribeSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeSlotType where
  toPath DescribeSlotType' {..} =
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

instance Core.ToQuery DescribeSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSlotTypeResponse' smart constructor.
data DescribeSlotTypeResponse = DescribeSlotTypeResponse'
  { -- | The built in slot type used as a parent to this slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | The values that the slot type can take. Includes any synonyms for the
    -- slot type values.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The strategy that Amazon Lex uses to choose a value from a list of
    -- possible values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the bot associated with the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The language and locale specified for the slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name specified for the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description specified for the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'describeSlotTypeResponse_parentSlotTypeSignature' - The built in slot type used as a parent to this slot type.
--
-- 'slotTypeValues', 'describeSlotTypeResponse_slotTypeValues' - The values that the slot type can take. Includes any synonyms for the
-- slot type values.
--
-- 'valueSelectionSetting', 'describeSlotTypeResponse_valueSelectionSetting' - The strategy that Amazon Lex uses to choose a value from a list of
-- possible values.
--
-- 'botVersion', 'describeSlotTypeResponse_botVersion' - The version of the bot associated with the slot type.
--
-- 'lastUpdatedDateTime', 'describeSlotTypeResponse_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
--
-- 'botId', 'describeSlotTypeResponse_botId' - The identifier of the bot associated with the slot type.
--
-- 'localeId', 'describeSlotTypeResponse_localeId' - The language and locale specified for the slot type.
--
-- 'creationDateTime', 'describeSlotTypeResponse_creationDateTime' - A timestamp of the date and time that the slot type was created.
--
-- 'slotTypeName', 'describeSlotTypeResponse_slotTypeName' - The name specified for the slot type.
--
-- 'description', 'describeSlotTypeResponse_description' - The description specified for the slot type.
--
-- 'slotTypeId', 'describeSlotTypeResponse_slotTypeId' - The unique identifier for the slot type.
--
-- 'httpStatus', 'describeSlotTypeResponse_httpStatus' - The response's http status code.
newDescribeSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSlotTypeResponse
newDescribeSlotTypeResponse pHttpStatus_ =
  DescribeSlotTypeResponse'
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

-- | The built in slot type used as a parent to this slot type.
describeSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\DescribeSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@DescribeSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: DescribeSlotTypeResponse)

-- | The values that the slot type can take. Includes any synonyms for the
-- slot type values.
describeSlotTypeResponse_slotTypeValues :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
describeSlotTypeResponse_slotTypeValues = Lens.lens (\DescribeSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeValues = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The strategy that Amazon Lex uses to choose a value from a list of
-- possible values.
describeSlotTypeResponse_valueSelectionSetting :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
describeSlotTypeResponse_valueSelectionSetting = Lens.lens (\DescribeSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@DescribeSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: DescribeSlotTypeResponse)

-- | The version of the bot associated with the slot type.
describeSlotTypeResponse_botVersion :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_botVersion = Lens.lens (\DescribeSlotTypeResponse' {botVersion} -> botVersion) (\s@DescribeSlotTypeResponse' {} a -> s {botVersion = a} :: DescribeSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was last updated.
describeSlotTypeResponse_lastUpdatedDateTime :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotTypeResponse_lastUpdatedDateTime = Lens.lens (\DescribeSlotTypeResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeSlotTypeResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The identifier of the bot associated with the slot type.
describeSlotTypeResponse_botId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_botId = Lens.lens (\DescribeSlotTypeResponse' {botId} -> botId) (\s@DescribeSlotTypeResponse' {} a -> s {botId = a} :: DescribeSlotTypeResponse)

-- | The language and locale specified for the slot type.
describeSlotTypeResponse_localeId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_localeId = Lens.lens (\DescribeSlotTypeResponse' {localeId} -> localeId) (\s@DescribeSlotTypeResponse' {} a -> s {localeId = a} :: DescribeSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was created.
describeSlotTypeResponse_creationDateTime :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotTypeResponse_creationDateTime = Lens.lens (\DescribeSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@DescribeSlotTypeResponse' {} a -> s {creationDateTime = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The name specified for the slot type.
describeSlotTypeResponse_slotTypeName :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_slotTypeName = Lens.lens (\DescribeSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeName = a} :: DescribeSlotTypeResponse)

-- | The description specified for the slot type.
describeSlotTypeResponse_description :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_description = Lens.lens (\DescribeSlotTypeResponse' {description} -> description) (\s@DescribeSlotTypeResponse' {} a -> s {description = a} :: DescribeSlotTypeResponse)

-- | The unique identifier for the slot type.
describeSlotTypeResponse_slotTypeId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_slotTypeId = Lens.lens (\DescribeSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeId = a} :: DescribeSlotTypeResponse)

-- | The response's http status code.
describeSlotTypeResponse_httpStatus :: Lens.Lens' DescribeSlotTypeResponse Prelude.Int
describeSlotTypeResponse_httpStatus = Lens.lens (\DescribeSlotTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeSlotTypeResponse' {} a -> s {httpStatus = a} :: DescribeSlotTypeResponse)

instance Prelude.NFData DescribeSlotTypeResponse
