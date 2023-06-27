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
-- Module      : Amazonka.FIS.Types.TargetResourceTypeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.TargetResourceTypeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource type.
--
-- /See:/ 'newTargetResourceTypeSummary' smart constructor.
data TargetResourceTypeSummary = TargetResourceTypeSummary'
  { -- | A description of the resource type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetResourceTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'targetResourceTypeSummary_description' - A description of the resource type.
--
-- 'resourceType', 'targetResourceTypeSummary_resourceType' - The resource type.
newTargetResourceTypeSummary ::
  TargetResourceTypeSummary
newTargetResourceTypeSummary =
  TargetResourceTypeSummary'
    { description =
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | A description of the resource type.
targetResourceTypeSummary_description :: Lens.Lens' TargetResourceTypeSummary (Prelude.Maybe Prelude.Text)
targetResourceTypeSummary_description = Lens.lens (\TargetResourceTypeSummary' {description} -> description) (\s@TargetResourceTypeSummary' {} a -> s {description = a} :: TargetResourceTypeSummary)

-- | The resource type.
targetResourceTypeSummary_resourceType :: Lens.Lens' TargetResourceTypeSummary (Prelude.Maybe Prelude.Text)
targetResourceTypeSummary_resourceType = Lens.lens (\TargetResourceTypeSummary' {resourceType} -> resourceType) (\s@TargetResourceTypeSummary' {} a -> s {resourceType = a} :: TargetResourceTypeSummary)

instance Data.FromJSON TargetResourceTypeSummary where
  parseJSON =
    Data.withObject
      "TargetResourceTypeSummary"
      ( \x ->
          TargetResourceTypeSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable TargetResourceTypeSummary where
  hashWithSalt _salt TargetResourceTypeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData TargetResourceTypeSummary where
  rnf TargetResourceTypeSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf resourceType
