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
-- Module      : Amazonka.LexV2Models.DescribeSlotType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about a slot type.
module Amazonka.LexV2Models.DescribeSlotType
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
    describeSlotTypeResponse_botVersion,
    describeSlotTypeResponse_compositeSlotTypeSetting,
    describeSlotTypeResponse_creationDateTime,
    describeSlotTypeResponse_localeId,
    describeSlotTypeResponse_externalSourceSetting,
    describeSlotTypeResponse_valueSelectionSetting,
    describeSlotTypeResponse_description,
    describeSlotTypeResponse_botId,
    describeSlotTypeResponse_slotTypeValues,
    describeSlotTypeResponse_slotTypeName,
    describeSlotTypeResponse_slotTypeId,
    describeSlotTypeResponse_parentSlotTypeSignature,
    describeSlotTypeResponse_lastUpdatedDateTime,
    describeSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSlotTypeResponse'
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
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSlotType where
  hashWithSalt _salt DescribeSlotType' {..} =
    _salt `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DescribeSlotType where
  rnf DescribeSlotType' {..} =
    Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

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
  { -- | The version of the bot associated with the slot type.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifications for a composite slot type.
    compositeSlotTypeSetting :: Prelude.Maybe CompositeSlotTypeSetting,
    -- | A timestamp of the date and time that the slot type was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The language and locale specified for the slot type.
    localeId :: Prelude.Maybe Prelude.Text,
    externalSourceSetting :: Prelude.Maybe ExternalSourceSetting,
    -- | The strategy that Amazon Lex uses to choose a value from a list of
    -- possible values.
    valueSelectionSetting :: Prelude.Maybe SlotValueSelectionSetting,
    -- | The description specified for the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot associated with the slot type.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The values that the slot type can take. Includes any synonyms for the
    -- slot type values.
    slotTypeValues :: Prelude.Maybe (Prelude.NonEmpty SlotTypeValue),
    -- | The name specified for the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | The built in slot type used as a parent to this slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
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
-- 'botVersion', 'describeSlotTypeResponse_botVersion' - The version of the bot associated with the slot type.
--
-- 'compositeSlotTypeSetting', 'describeSlotTypeResponse_compositeSlotTypeSetting' - Specifications for a composite slot type.
--
-- 'creationDateTime', 'describeSlotTypeResponse_creationDateTime' - A timestamp of the date and time that the slot type was created.
--
-- 'localeId', 'describeSlotTypeResponse_localeId' - The language and locale specified for the slot type.
--
-- 'externalSourceSetting', 'describeSlotTypeResponse_externalSourceSetting' - Undocumented member.
--
-- 'valueSelectionSetting', 'describeSlotTypeResponse_valueSelectionSetting' - The strategy that Amazon Lex uses to choose a value from a list of
-- possible values.
--
-- 'description', 'describeSlotTypeResponse_description' - The description specified for the slot type.
--
-- 'botId', 'describeSlotTypeResponse_botId' - The identifier of the bot associated with the slot type.
--
-- 'slotTypeValues', 'describeSlotTypeResponse_slotTypeValues' - The values that the slot type can take. Includes any synonyms for the
-- slot type values.
--
-- 'slotTypeName', 'describeSlotTypeResponse_slotTypeName' - The name specified for the slot type.
--
-- 'slotTypeId', 'describeSlotTypeResponse_slotTypeId' - The unique identifier for the slot type.
--
-- 'parentSlotTypeSignature', 'describeSlotTypeResponse_parentSlotTypeSignature' - The built in slot type used as a parent to this slot type.
--
-- 'lastUpdatedDateTime', 'describeSlotTypeResponse_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
--
-- 'httpStatus', 'describeSlotTypeResponse_httpStatus' - The response's http status code.
newDescribeSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSlotTypeResponse
newDescribeSlotTypeResponse pHttpStatus_ =
  DescribeSlotTypeResponse'
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
      lastUpdatedDateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot associated with the slot type.
describeSlotTypeResponse_botVersion :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_botVersion = Lens.lens (\DescribeSlotTypeResponse' {botVersion} -> botVersion) (\s@DescribeSlotTypeResponse' {} a -> s {botVersion = a} :: DescribeSlotTypeResponse)

-- | Specifications for a composite slot type.
describeSlotTypeResponse_compositeSlotTypeSetting :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe CompositeSlotTypeSetting)
describeSlotTypeResponse_compositeSlotTypeSetting = Lens.lens (\DescribeSlotTypeResponse' {compositeSlotTypeSetting} -> compositeSlotTypeSetting) (\s@DescribeSlotTypeResponse' {} a -> s {compositeSlotTypeSetting = a} :: DescribeSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was created.
describeSlotTypeResponse_creationDateTime :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotTypeResponse_creationDateTime = Lens.lens (\DescribeSlotTypeResponse' {creationDateTime} -> creationDateTime) (\s@DescribeSlotTypeResponse' {} a -> s {creationDateTime = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The language and locale specified for the slot type.
describeSlotTypeResponse_localeId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_localeId = Lens.lens (\DescribeSlotTypeResponse' {localeId} -> localeId) (\s@DescribeSlotTypeResponse' {} a -> s {localeId = a} :: DescribeSlotTypeResponse)

-- | Undocumented member.
describeSlotTypeResponse_externalSourceSetting :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe ExternalSourceSetting)
describeSlotTypeResponse_externalSourceSetting = Lens.lens (\DescribeSlotTypeResponse' {externalSourceSetting} -> externalSourceSetting) (\s@DescribeSlotTypeResponse' {} a -> s {externalSourceSetting = a} :: DescribeSlotTypeResponse)

-- | The strategy that Amazon Lex uses to choose a value from a list of
-- possible values.
describeSlotTypeResponse_valueSelectionSetting :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe SlotValueSelectionSetting)
describeSlotTypeResponse_valueSelectionSetting = Lens.lens (\DescribeSlotTypeResponse' {valueSelectionSetting} -> valueSelectionSetting) (\s@DescribeSlotTypeResponse' {} a -> s {valueSelectionSetting = a} :: DescribeSlotTypeResponse)

-- | The description specified for the slot type.
describeSlotTypeResponse_description :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_description = Lens.lens (\DescribeSlotTypeResponse' {description} -> description) (\s@DescribeSlotTypeResponse' {} a -> s {description = a} :: DescribeSlotTypeResponse)

-- | The identifier of the bot associated with the slot type.
describeSlotTypeResponse_botId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_botId = Lens.lens (\DescribeSlotTypeResponse' {botId} -> botId) (\s@DescribeSlotTypeResponse' {} a -> s {botId = a} :: DescribeSlotTypeResponse)

-- | The values that the slot type can take. Includes any synonyms for the
-- slot type values.
describeSlotTypeResponse_slotTypeValues :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe (Prelude.NonEmpty SlotTypeValue))
describeSlotTypeResponse_slotTypeValues = Lens.lens (\DescribeSlotTypeResponse' {slotTypeValues} -> slotTypeValues) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeValues = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name specified for the slot type.
describeSlotTypeResponse_slotTypeName :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_slotTypeName = Lens.lens (\DescribeSlotTypeResponse' {slotTypeName} -> slotTypeName) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeName = a} :: DescribeSlotTypeResponse)

-- | The unique identifier for the slot type.
describeSlotTypeResponse_slotTypeId :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_slotTypeId = Lens.lens (\DescribeSlotTypeResponse' {slotTypeId} -> slotTypeId) (\s@DescribeSlotTypeResponse' {} a -> s {slotTypeId = a} :: DescribeSlotTypeResponse)

-- | The built in slot type used as a parent to this slot type.
describeSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.Text)
describeSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\DescribeSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@DescribeSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: DescribeSlotTypeResponse)

-- | A timestamp of the date and time that the slot type was last updated.
describeSlotTypeResponse_lastUpdatedDateTime :: Lens.Lens' DescribeSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeSlotTypeResponse_lastUpdatedDateTime = Lens.lens (\DescribeSlotTypeResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeSlotTypeResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeSlotTypeResponse_httpStatus :: Lens.Lens' DescribeSlotTypeResponse Prelude.Int
describeSlotTypeResponse_httpStatus = Lens.lens (\DescribeSlotTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeSlotTypeResponse' {} a -> s {httpStatus = a} :: DescribeSlotTypeResponse)

instance Prelude.NFData DescribeSlotTypeResponse where
  rnf DescribeSlotTypeResponse' {..} =
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
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf httpStatus
