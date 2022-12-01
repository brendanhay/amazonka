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
-- Module      : Amazonka.ComputeOptimizer.Types.ExportDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExportDestination where

import Amazonka.ComputeOptimizer.Types.S3Destination
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination of the recommendations export and metadata
-- files.
--
-- /See:/ 'newExportDestination' smart constructor.
data ExportDestination = ExportDestination'
  { -- | An object that describes the destination Amazon Simple Storage Service
    -- (Amazon S3) bucket name and object keys of a recommendations export
    -- file, and its associated metadata file.
    s3 :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'exportDestination_s3' - An object that describes the destination Amazon Simple Storage Service
-- (Amazon S3) bucket name and object keys of a recommendations export
-- file, and its associated metadata file.
newExportDestination ::
  ExportDestination
newExportDestination =
  ExportDestination' {s3 = Prelude.Nothing}

-- | An object that describes the destination Amazon Simple Storage Service
-- (Amazon S3) bucket name and object keys of a recommendations export
-- file, and its associated metadata file.
exportDestination_s3 :: Lens.Lens' ExportDestination (Prelude.Maybe S3Destination)
exportDestination_s3 = Lens.lens (\ExportDestination' {s3} -> s3) (\s@ExportDestination' {} a -> s {s3 = a} :: ExportDestination)

instance Core.FromJSON ExportDestination where
  parseJSON =
    Core.withObject
      "ExportDestination"
      ( \x ->
          ExportDestination' Prelude.<$> (x Core..:? "s3")
      )

instance Prelude.Hashable ExportDestination where
  hashWithSalt _salt ExportDestination' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData ExportDestination where
  rnf ExportDestination' {..} = Prelude.rnf s3
