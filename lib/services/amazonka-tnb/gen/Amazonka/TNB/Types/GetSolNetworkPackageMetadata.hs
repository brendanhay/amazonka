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
-- Module      : Amazonka.TNB.Types.GetSolNetworkPackageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolNetworkPackageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.NetworkArtifactMeta

-- | Metadata associated with a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- /See:/ 'newGetSolNetworkPackageMetadata' smart constructor.
data GetSolNetworkPackageMetadata = GetSolNetworkPackageMetadata'
  { -- | Metadata related to the onboarded network service descriptor in the
    -- network package.
    nsd :: Prelude.Maybe NetworkArtifactMeta,
    -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsd', 'getSolNetworkPackageMetadata_nsd' - Metadata related to the onboarded network service descriptor in the
-- network package.
--
-- 'createdAt', 'getSolNetworkPackageMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'getSolNetworkPackageMetadata_lastModified' - The date that the resource was last modified.
newGetSolNetworkPackageMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  GetSolNetworkPackageMetadata
newGetSolNetworkPackageMetadata
  pCreatedAt_
  pLastModified_ =
    GetSolNetworkPackageMetadata'
      { nsd =
          Prelude.Nothing,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | Metadata related to the onboarded network service descriptor in the
-- network package.
getSolNetworkPackageMetadata_nsd :: Lens.Lens' GetSolNetworkPackageMetadata (Prelude.Maybe NetworkArtifactMeta)
getSolNetworkPackageMetadata_nsd = Lens.lens (\GetSolNetworkPackageMetadata' {nsd} -> nsd) (\s@GetSolNetworkPackageMetadata' {} a -> s {nsd = a} :: GetSolNetworkPackageMetadata)

-- | The date that the resource was created.
getSolNetworkPackageMetadata_createdAt :: Lens.Lens' GetSolNetworkPackageMetadata Prelude.UTCTime
getSolNetworkPackageMetadata_createdAt = Lens.lens (\GetSolNetworkPackageMetadata' {createdAt} -> createdAt) (\s@GetSolNetworkPackageMetadata' {} a -> s {createdAt = a} :: GetSolNetworkPackageMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
getSolNetworkPackageMetadata_lastModified :: Lens.Lens' GetSolNetworkPackageMetadata Prelude.UTCTime
getSolNetworkPackageMetadata_lastModified = Lens.lens (\GetSolNetworkPackageMetadata' {lastModified} -> lastModified) (\s@GetSolNetworkPackageMetadata' {} a -> s {lastModified = a} :: GetSolNetworkPackageMetadata) Prelude.. Data._Time

instance Data.FromJSON GetSolNetworkPackageMetadata where
  parseJSON =
    Data.withObject
      "GetSolNetworkPackageMetadata"
      ( \x ->
          GetSolNetworkPackageMetadata'
            Prelude.<$> (x Data..:? "nsd")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    GetSolNetworkPackageMetadata
  where
  hashWithSalt _salt GetSolNetworkPackageMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` nsd
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData GetSolNetworkPackageMetadata where
  rnf GetSolNetworkPackageMetadata' {..} =
    Prelude.rnf nsd
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
