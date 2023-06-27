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
-- Module      : Amazonka.TNB.Types.GetSolFunctionPackageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolFunctionPackageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.FunctionArtifactMeta

-- | Metadata related to the function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- /See:/ 'newGetSolFunctionPackageMetadata' smart constructor.
data GetSolFunctionPackageMetadata = GetSolFunctionPackageMetadata'
  { -- | Metadata related to the function package descriptor of the function
    -- package.
    vnfd :: Prelude.Maybe FunctionArtifactMeta,
    -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfd', 'getSolFunctionPackageMetadata_vnfd' - Metadata related to the function package descriptor of the function
-- package.
--
-- 'createdAt', 'getSolFunctionPackageMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'getSolFunctionPackageMetadata_lastModified' - The date that the resource was last modified.
newGetSolFunctionPackageMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  GetSolFunctionPackageMetadata
newGetSolFunctionPackageMetadata
  pCreatedAt_
  pLastModified_ =
    GetSolFunctionPackageMetadata'
      { vnfd =
          Prelude.Nothing,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | Metadata related to the function package descriptor of the function
-- package.
getSolFunctionPackageMetadata_vnfd :: Lens.Lens' GetSolFunctionPackageMetadata (Prelude.Maybe FunctionArtifactMeta)
getSolFunctionPackageMetadata_vnfd = Lens.lens (\GetSolFunctionPackageMetadata' {vnfd} -> vnfd) (\s@GetSolFunctionPackageMetadata' {} a -> s {vnfd = a} :: GetSolFunctionPackageMetadata)

-- | The date that the resource was created.
getSolFunctionPackageMetadata_createdAt :: Lens.Lens' GetSolFunctionPackageMetadata Prelude.UTCTime
getSolFunctionPackageMetadata_createdAt = Lens.lens (\GetSolFunctionPackageMetadata' {createdAt} -> createdAt) (\s@GetSolFunctionPackageMetadata' {} a -> s {createdAt = a} :: GetSolFunctionPackageMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
getSolFunctionPackageMetadata_lastModified :: Lens.Lens' GetSolFunctionPackageMetadata Prelude.UTCTime
getSolFunctionPackageMetadata_lastModified = Lens.lens (\GetSolFunctionPackageMetadata' {lastModified} -> lastModified) (\s@GetSolFunctionPackageMetadata' {} a -> s {lastModified = a} :: GetSolFunctionPackageMetadata) Prelude.. Data._Time

instance Data.FromJSON GetSolFunctionPackageMetadata where
  parseJSON =
    Data.withObject
      "GetSolFunctionPackageMetadata"
      ( \x ->
          GetSolFunctionPackageMetadata'
            Prelude.<$> (x Data..:? "vnfd")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    GetSolFunctionPackageMetadata
  where
  hashWithSalt _salt GetSolFunctionPackageMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` vnfd
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData GetSolFunctionPackageMetadata where
  rnf GetSolFunctionPackageMetadata' {..} =
    Prelude.rnf vnfd
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
