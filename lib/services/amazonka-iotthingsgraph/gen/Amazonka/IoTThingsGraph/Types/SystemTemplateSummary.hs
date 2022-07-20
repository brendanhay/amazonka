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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemTemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a system.
--
-- /See:/ 'newSystemTemplateSummary' smart constructor.
data SystemTemplateSummary = SystemTemplateSummary'
  { -- | The revision number of the system.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the system.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the system.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date when the system was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionNumber', 'systemTemplateSummary_revisionNumber' - The revision number of the system.
--
-- 'arn', 'systemTemplateSummary_arn' - The ARN of the system.
--
-- 'id', 'systemTemplateSummary_id' - The ID of the system.
--
-- 'createdAt', 'systemTemplateSummary_createdAt' - The date when the system was created.
newSystemTemplateSummary ::
  SystemTemplateSummary
newSystemTemplateSummary =
  SystemTemplateSummary'
    { revisionNumber =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The revision number of the system.
systemTemplateSummary_revisionNumber :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Integer)
systemTemplateSummary_revisionNumber = Lens.lens (\SystemTemplateSummary' {revisionNumber} -> revisionNumber) (\s@SystemTemplateSummary' {} a -> s {revisionNumber = a} :: SystemTemplateSummary)

-- | The ARN of the system.
systemTemplateSummary_arn :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Text)
systemTemplateSummary_arn = Lens.lens (\SystemTemplateSummary' {arn} -> arn) (\s@SystemTemplateSummary' {} a -> s {arn = a} :: SystemTemplateSummary)

-- | The ID of the system.
systemTemplateSummary_id :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Text)
systemTemplateSummary_id = Lens.lens (\SystemTemplateSummary' {id} -> id) (\s@SystemTemplateSummary' {} a -> s {id = a} :: SystemTemplateSummary)

-- | The date when the system was created.
systemTemplateSummary_createdAt :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.UTCTime)
systemTemplateSummary_createdAt = Lens.lens (\SystemTemplateSummary' {createdAt} -> createdAt) (\s@SystemTemplateSummary' {} a -> s {createdAt = a} :: SystemTemplateSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SystemTemplateSummary where
  parseJSON =
    Core.withObject
      "SystemTemplateSummary"
      ( \x ->
          SystemTemplateSummary'
            Prelude.<$> (x Core..:? "revisionNumber")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable SystemTemplateSummary where
  hashWithSalt _salt SystemTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` revisionNumber
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData SystemTemplateSummary where
  rnf SystemTemplateSummary' {..} =
    Prelude.rnf revisionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
