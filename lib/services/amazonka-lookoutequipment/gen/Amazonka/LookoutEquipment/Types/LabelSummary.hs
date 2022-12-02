{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutEquipment.Types.LabelSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LabelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.LabelRating
import qualified Amazonka.Prelude as Prelude

-- | Information about the label.
--
-- /See:/ 'newLabelSummary' smart constructor.
data LabelSummary = LabelSummary'
  { -- | Indicates that a label pertains to a particular piece of equipment.
    equipment :: Prelude.Maybe Prelude.Text,
    -- | The ID of the label.
    labelId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp indicating the end of the label.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates the type of anomaly associated with the label.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    faultCode :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the label group.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a labeled event represents an anomaly.
    rating :: Prelude.Maybe LabelRating,
    -- | The time at which the label was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The timestamp indicating the start of the label.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the label group.
    labelGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'equipment', 'labelSummary_equipment' - Indicates that a label pertains to a particular piece of equipment.
--
-- 'labelId', 'labelSummary_labelId' - The ID of the label.
--
-- 'endTime', 'labelSummary_endTime' - The timestamp indicating the end of the label.
--
-- 'faultCode', 'labelSummary_faultCode' - Indicates the type of anomaly associated with the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'labelGroupArn', 'labelSummary_labelGroupArn' - The ARN of the label group.
--
-- 'rating', 'labelSummary_rating' - Indicates whether a labeled event represents an anomaly.
--
-- 'createdAt', 'labelSummary_createdAt' - The time at which the label was created.
--
-- 'startTime', 'labelSummary_startTime' - The timestamp indicating the start of the label.
--
-- 'labelGroupName', 'labelSummary_labelGroupName' - The name of the label group.
newLabelSummary ::
  LabelSummary
newLabelSummary =
  LabelSummary'
    { equipment = Prelude.Nothing,
      labelId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      faultCode = Prelude.Nothing,
      labelGroupArn = Prelude.Nothing,
      rating = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      startTime = Prelude.Nothing,
      labelGroupName = Prelude.Nothing
    }

-- | Indicates that a label pertains to a particular piece of equipment.
labelSummary_equipment :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_equipment = Lens.lens (\LabelSummary' {equipment} -> equipment) (\s@LabelSummary' {} a -> s {equipment = a} :: LabelSummary)

-- | The ID of the label.
labelSummary_labelId :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_labelId = Lens.lens (\LabelSummary' {labelId} -> labelId) (\s@LabelSummary' {} a -> s {labelId = a} :: LabelSummary)

-- | The timestamp indicating the end of the label.
labelSummary_endTime :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.UTCTime)
labelSummary_endTime = Lens.lens (\LabelSummary' {endTime} -> endTime) (\s@LabelSummary' {} a -> s {endTime = a} :: LabelSummary) Prelude.. Lens.mapping Data._Time

-- | Indicates the type of anomaly associated with the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
labelSummary_faultCode :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_faultCode = Lens.lens (\LabelSummary' {faultCode} -> faultCode) (\s@LabelSummary' {} a -> s {faultCode = a} :: LabelSummary)

-- | The ARN of the label group.
labelSummary_labelGroupArn :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_labelGroupArn = Lens.lens (\LabelSummary' {labelGroupArn} -> labelGroupArn) (\s@LabelSummary' {} a -> s {labelGroupArn = a} :: LabelSummary)

-- | Indicates whether a labeled event represents an anomaly.
labelSummary_rating :: Lens.Lens' LabelSummary (Prelude.Maybe LabelRating)
labelSummary_rating = Lens.lens (\LabelSummary' {rating} -> rating) (\s@LabelSummary' {} a -> s {rating = a} :: LabelSummary)

-- | The time at which the label was created.
labelSummary_createdAt :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.UTCTime)
labelSummary_createdAt = Lens.lens (\LabelSummary' {createdAt} -> createdAt) (\s@LabelSummary' {} a -> s {createdAt = a} :: LabelSummary) Prelude.. Lens.mapping Data._Time

-- | The timestamp indicating the start of the label.
labelSummary_startTime :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.UTCTime)
labelSummary_startTime = Lens.lens (\LabelSummary' {startTime} -> startTime) (\s@LabelSummary' {} a -> s {startTime = a} :: LabelSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the label group.
labelSummary_labelGroupName :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_labelGroupName = Lens.lens (\LabelSummary' {labelGroupName} -> labelGroupName) (\s@LabelSummary' {} a -> s {labelGroupName = a} :: LabelSummary)

instance Data.FromJSON LabelSummary where
  parseJSON =
    Data.withObject
      "LabelSummary"
      ( \x ->
          LabelSummary'
            Prelude.<$> (x Data..:? "Equipment")
            Prelude.<*> (x Data..:? "LabelId")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "FaultCode")
            Prelude.<*> (x Data..:? "LabelGroupArn")
            Prelude.<*> (x Data..:? "Rating")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "LabelGroupName")
      )

instance Prelude.Hashable LabelSummary where
  hashWithSalt _salt LabelSummary' {..} =
    _salt `Prelude.hashWithSalt` equipment
      `Prelude.hashWithSalt` labelId
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` faultCode
      `Prelude.hashWithSalt` labelGroupArn
      `Prelude.hashWithSalt` rating
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData LabelSummary where
  rnf LabelSummary' {..} =
    Prelude.rnf equipment
      `Prelude.seq` Prelude.rnf labelId
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf faultCode
      `Prelude.seq` Prelude.rnf labelGroupArn
      `Prelude.seq` Prelude.rnf rating
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf labelGroupName
