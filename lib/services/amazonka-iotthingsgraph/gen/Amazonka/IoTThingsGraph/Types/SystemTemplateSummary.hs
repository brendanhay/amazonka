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
  { -- | The ARN of the system.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the system was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The revision number of the system.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the system.
    id :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'systemTemplateSummary_arn' - The ARN of the system.
--
-- 'createdAt', 'systemTemplateSummary_createdAt' - The date when the system was created.
--
-- 'revisionNumber', 'systemTemplateSummary_revisionNumber' - The revision number of the system.
--
-- 'id', 'systemTemplateSummary_id' - The ID of the system.
newSystemTemplateSummary ::
  SystemTemplateSummary
newSystemTemplateSummary =
  SystemTemplateSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      revisionNumber = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The ARN of the system.
systemTemplateSummary_arn :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Text)
systemTemplateSummary_arn = Lens.lens (\SystemTemplateSummary' {arn} -> arn) (\s@SystemTemplateSummary' {} a -> s {arn = a} :: SystemTemplateSummary)

-- | The date when the system was created.
systemTemplateSummary_createdAt :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.UTCTime)
systemTemplateSummary_createdAt = Lens.lens (\SystemTemplateSummary' {createdAt} -> createdAt) (\s@SystemTemplateSummary' {} a -> s {createdAt = a} :: SystemTemplateSummary) Prelude.. Lens.mapping Core._Time

-- | The revision number of the system.
systemTemplateSummary_revisionNumber :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Integer)
systemTemplateSummary_revisionNumber = Lens.lens (\SystemTemplateSummary' {revisionNumber} -> revisionNumber) (\s@SystemTemplateSummary' {} a -> s {revisionNumber = a} :: SystemTemplateSummary)

-- | The ID of the system.
systemTemplateSummary_id :: Lens.Lens' SystemTemplateSummary (Prelude.Maybe Prelude.Text)
systemTemplateSummary_id = Lens.lens (\SystemTemplateSummary' {id} -> id) (\s@SystemTemplateSummary' {} a -> s {id = a} :: SystemTemplateSummary)

instance Core.FromJSON SystemTemplateSummary where
  parseJSON =
    Core.withObject
      "SystemTemplateSummary"
      ( \x ->
          SystemTemplateSummary'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "revisionNumber")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable SystemTemplateSummary

instance Prelude.NFData SystemTemplateSummary
