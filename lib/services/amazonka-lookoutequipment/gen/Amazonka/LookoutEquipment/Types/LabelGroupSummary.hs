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
-- Module      : Amazonka.LookoutEquipment.Types.LabelGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LabelGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the label group.
--
-- /See:/ 'newLabelGroupSummary' smart constructor.
data LabelGroupSummary = LabelGroupSummary'
  { -- | The ARN of the label group.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the label group was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The time at which the label group was updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the label group.
    labelGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupArn', 'labelGroupSummary_labelGroupArn' - The ARN of the label group.
--
-- 'createdAt', 'labelGroupSummary_createdAt' - The time at which the label group was created.
--
-- 'updatedAt', 'labelGroupSummary_updatedAt' - The time at which the label group was updated.
--
-- 'labelGroupName', 'labelGroupSummary_labelGroupName' - The name of the label group.
newLabelGroupSummary ::
  LabelGroupSummary
newLabelGroupSummary =
  LabelGroupSummary'
    { labelGroupArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      labelGroupName = Prelude.Nothing
    }

-- | The ARN of the label group.
labelGroupSummary_labelGroupArn :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.Text)
labelGroupSummary_labelGroupArn = Lens.lens (\LabelGroupSummary' {labelGroupArn} -> labelGroupArn) (\s@LabelGroupSummary' {} a -> s {labelGroupArn = a} :: LabelGroupSummary)

-- | The time at which the label group was created.
labelGroupSummary_createdAt :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.UTCTime)
labelGroupSummary_createdAt = Lens.lens (\LabelGroupSummary' {createdAt} -> createdAt) (\s@LabelGroupSummary' {} a -> s {createdAt = a} :: LabelGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which the label group was updated.
labelGroupSummary_updatedAt :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.UTCTime)
labelGroupSummary_updatedAt = Lens.lens (\LabelGroupSummary' {updatedAt} -> updatedAt) (\s@LabelGroupSummary' {} a -> s {updatedAt = a} :: LabelGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the label group.
labelGroupSummary_labelGroupName :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.Text)
labelGroupSummary_labelGroupName = Lens.lens (\LabelGroupSummary' {labelGroupName} -> labelGroupName) (\s@LabelGroupSummary' {} a -> s {labelGroupName = a} :: LabelGroupSummary)

instance Data.FromJSON LabelGroupSummary where
  parseJSON =
    Data.withObject
      "LabelGroupSummary"
      ( \x ->
          LabelGroupSummary'
            Prelude.<$> (x Data..:? "LabelGroupArn")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "LabelGroupName")
      )

instance Prelude.Hashable LabelGroupSummary where
  hashWithSalt _salt LabelGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` labelGroupArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData LabelGroupSummary where
  rnf LabelGroupSummary' {..} =
    Prelude.rnf labelGroupArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf labelGroupName
