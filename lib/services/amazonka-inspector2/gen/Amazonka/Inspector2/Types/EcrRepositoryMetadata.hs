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
-- Module      : Amazonka.Inspector2.Types.EcrRepositoryMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrRepositoryMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.EcrScanFrequency
import qualified Amazonka.Prelude as Prelude

-- | Information on the Amazon ECR repository metadata associated with a
-- finding.
--
-- /See:/ 'newEcrRepositoryMetadata' smart constructor.
data EcrRepositoryMetadata = EcrRepositoryMetadata'
  { -- | The name of the Amazon ECR repository.
    name :: Prelude.Maybe Prelude.Text,
    -- | The frequency of scans.
    scanFrequency :: Prelude.Maybe EcrScanFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrRepositoryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ecrRepositoryMetadata_name' - The name of the Amazon ECR repository.
--
-- 'scanFrequency', 'ecrRepositoryMetadata_scanFrequency' - The frequency of scans.
newEcrRepositoryMetadata ::
  EcrRepositoryMetadata
newEcrRepositoryMetadata =
  EcrRepositoryMetadata'
    { name = Prelude.Nothing,
      scanFrequency = Prelude.Nothing
    }

-- | The name of the Amazon ECR repository.
ecrRepositoryMetadata_name :: Lens.Lens' EcrRepositoryMetadata (Prelude.Maybe Prelude.Text)
ecrRepositoryMetadata_name = Lens.lens (\EcrRepositoryMetadata' {name} -> name) (\s@EcrRepositoryMetadata' {} a -> s {name = a} :: EcrRepositoryMetadata)

-- | The frequency of scans.
ecrRepositoryMetadata_scanFrequency :: Lens.Lens' EcrRepositoryMetadata (Prelude.Maybe EcrScanFrequency)
ecrRepositoryMetadata_scanFrequency = Lens.lens (\EcrRepositoryMetadata' {scanFrequency} -> scanFrequency) (\s@EcrRepositoryMetadata' {} a -> s {scanFrequency = a} :: EcrRepositoryMetadata)

instance Data.FromJSON EcrRepositoryMetadata where
  parseJSON =
    Data.withObject
      "EcrRepositoryMetadata"
      ( \x ->
          EcrRepositoryMetadata'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "scanFrequency")
      )

instance Prelude.Hashable EcrRepositoryMetadata where
  hashWithSalt _salt EcrRepositoryMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scanFrequency

instance Prelude.NFData EcrRepositoryMetadata where
  rnf EcrRepositoryMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scanFrequency
