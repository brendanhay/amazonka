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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.FrameworkMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a framework, such as the name, ID, or description.
--
-- /See:/ 'newFrameworkMetadata' smart constructor.
data FrameworkMetadata = FrameworkMetadata'
  { -- | The compliance standard that\'s associated with the framework. For
    -- example, this could be PCI DSS or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The description of the framework.
    description :: Prelude.Maybe Prelude.Text,
    -- | The logo that\'s associated with the framework.
    logo :: Prelude.Maybe Prelude.Text,
    -- | The name of the framework.
    name :: Prelude.Maybe Prelude.Text
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
-- 'complianceType', 'frameworkMetadata_complianceType' - The compliance standard that\'s associated with the framework. For
-- example, this could be PCI DSS or HIPAA.
--
-- 'description', 'frameworkMetadata_description' - The description of the framework.
--
-- 'logo', 'frameworkMetadata_logo' - The logo that\'s associated with the framework.
--
-- 'name', 'frameworkMetadata_name' - The name of the framework.
newFrameworkMetadata ::
  FrameworkMetadata
newFrameworkMetadata =
  FrameworkMetadata'
    { complianceType =
        Prelude.Nothing,
      description = Prelude.Nothing,
      logo = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The compliance standard that\'s associated with the framework. For
-- example, this could be PCI DSS or HIPAA.
frameworkMetadata_complianceType :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_complianceType = Lens.lens (\FrameworkMetadata' {complianceType} -> complianceType) (\s@FrameworkMetadata' {} a -> s {complianceType = a} :: FrameworkMetadata)

-- | The description of the framework.
frameworkMetadata_description :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_description = Lens.lens (\FrameworkMetadata' {description} -> description) (\s@FrameworkMetadata' {} a -> s {description = a} :: FrameworkMetadata)

-- | The logo that\'s associated with the framework.
frameworkMetadata_logo :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_logo = Lens.lens (\FrameworkMetadata' {logo} -> logo) (\s@FrameworkMetadata' {} a -> s {logo = a} :: FrameworkMetadata)

-- | The name of the framework.
frameworkMetadata_name :: Lens.Lens' FrameworkMetadata (Prelude.Maybe Prelude.Text)
frameworkMetadata_name = Lens.lens (\FrameworkMetadata' {name} -> name) (\s@FrameworkMetadata' {} a -> s {name = a} :: FrameworkMetadata)

instance Data.FromJSON FrameworkMetadata where
  parseJSON =
    Data.withObject
      "FrameworkMetadata"
      ( \x ->
          FrameworkMetadata'
            Prelude.<$> (x Data..:? "complianceType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "logo")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable FrameworkMetadata where
  hashWithSalt _salt FrameworkMetadata' {..} =
    _salt `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` logo
      `Prelude.hashWithSalt` name

instance Prelude.NFData FrameworkMetadata where
  rnf FrameworkMetadata' {..} =
    Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf logo
      `Prelude.seq` Prelude.rnf name
