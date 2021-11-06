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
-- Module      : Amazonka.AuditManager.Types.FrameworkMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.FrameworkMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a framework, such as the name, ID, description, and so
-- on.
--
-- /See:/ 'newFrameworkMetadata' smart constructor.
data FrameworkMetadata = FrameworkMetadata'
  { -- | The name of the framework.
    name :: Prelude.Maybe Prelude.Text,
    -- | The compliance standard associated with the framework, such as PCI-DSS
    -- or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The logo associated with the framework.
    logo :: Prelude.Maybe Prelude.Text,
    -- | The description of the framework.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameworkMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'frameworkMetadata_name' - The name of the framework.
--
-- 'complianceType', 'frameworkMetadata_complianceType' - The compliance standard associated with the framework, such as PCI-DSS
-- or HIPAA.
--
-- 'logo', 'frameworkMetadata_logo' - The logo associated with the framework.
--
-- 'description', 'frameworkMetadata_description' - The description of the framework.
newFrameworkMetadata ::
  FrameworkMetadata
newFrameworkMetadata =
  FrameworkMetadata'
    { name = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      logo = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the framework.
frameworkMetadata_name :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_name = Lens.lens (\FrameworkMetadata' {name} -> name) (\s@FrameworkMetadata' {} a -> s {name = a} :: FrameworkMetadata)

-- | The compliance standard associated with the framework, such as PCI-DSS
-- or HIPAA.
frameworkMetadata_complianceType :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_complianceType = Lens.lens (\FrameworkMetadata' {complianceType} -> complianceType) (\s@FrameworkMetadata' {} a -> s {complianceType = a} :: FrameworkMetadata)

-- | The logo associated with the framework.
frameworkMetadata_logo :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_logo = Lens.lens (\FrameworkMetadata' {logo} -> logo) (\s@FrameworkMetadata' {} a -> s {logo = a} :: FrameworkMetadata)

-- | The description of the framework.
frameworkMetadata_description :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_description = Lens.lens (\FrameworkMetadata' {description} -> description) (\s@FrameworkMetadata' {} a -> s {description = a} :: FrameworkMetadata)

instance Core.FromJSON FrameworkMetadata where
  parseJSON =
    Core.withObject
      "FrameworkMetadata"
      ( \x ->
          FrameworkMetadata'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "logo")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable FrameworkMetadata

instance Prelude.NFData FrameworkMetadata
