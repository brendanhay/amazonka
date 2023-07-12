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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The time at which the label group was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the label group.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the label group.
    labelGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the label group was updated.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'createdAt', 'labelGroupSummary_createdAt' - The time at which the label group was created.
--
-- 'labelGroupArn', 'labelGroupSummary_labelGroupArn' - The ARN of the label group.
--
-- 'labelGroupName', 'labelGroupSummary_labelGroupName' - The name of the label group.
--
-- 'updatedAt', 'labelGroupSummary_updatedAt' - The time at which the label group was updated.
newLabelGroupSummary ::
  LabelGroupSummary
newLabelGroupSummary =
  LabelGroupSummary'
    { createdAt = Prelude.Nothing,
      labelGroupArn = Prelude.Nothing,
      labelGroupName = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The time at which the label group was created.
labelGroupSummary_createdAt :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.UTCTime)
labelGroupSummary_createdAt = Lens.lens (\LabelGroupSummary' {createdAt} -> createdAt) (\s@LabelGroupSummary' {} a -> s {createdAt = a} :: LabelGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the label group.
labelGroupSummary_labelGroupArn :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.Text)
labelGroupSummary_labelGroupArn = Lens.lens (\LabelGroupSummary' {labelGroupArn} -> labelGroupArn) (\s@LabelGroupSummary' {} a -> s {labelGroupArn = a} :: LabelGroupSummary)

-- | The name of the label group.
labelGroupSummary_labelGroupName :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.Text)
labelGroupSummary_labelGroupName = Lens.lens (\LabelGroupSummary' {labelGroupName} -> labelGroupName) (\s@LabelGroupSummary' {} a -> s {labelGroupName = a} :: LabelGroupSummary)

-- | The time at which the label group was updated.
labelGroupSummary_updatedAt :: Lens.Lens' LabelGroupSummary (Prelude.Maybe Prelude.UTCTime)
labelGroupSummary_updatedAt = Lens.lens (\LabelGroupSummary' {updatedAt} -> updatedAt) (\s@LabelGroupSummary' {} a -> s {updatedAt = a} :: LabelGroupSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LabelGroupSummary where
  parseJSON =
    Data.withObject
      "LabelGroupSummary"
      ( \x ->
          LabelGroupSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "LabelGroupArn")
            Prelude.<*> (x Data..:? "LabelGroupName")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable LabelGroupSummary where
  hashWithSalt _salt LabelGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` labelGroupArn
      `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData LabelGroupSummary where
  rnf LabelGroupSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf labelGroupArn
      `Prelude.seq` Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf updatedAt
