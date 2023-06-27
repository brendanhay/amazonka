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
-- Module      : Amazonka.TNB.Types.ListSolFunctionPackageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolFunctionPackageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for the function package metadata.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- /See:/ 'newListSolFunctionPackageMetadata' smart constructor.
data ListSolFunctionPackageMetadata = ListSolFunctionPackageMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionPackageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listSolFunctionPackageMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'listSolFunctionPackageMetadata_lastModified' - The date that the resource was last modified.
newListSolFunctionPackageMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  ListSolFunctionPackageMetadata
newListSolFunctionPackageMetadata
  pCreatedAt_
  pLastModified_ =
    ListSolFunctionPackageMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
listSolFunctionPackageMetadata_createdAt :: Lens.Lens' ListSolFunctionPackageMetadata Prelude.UTCTime
listSolFunctionPackageMetadata_createdAt = Lens.lens (\ListSolFunctionPackageMetadata' {createdAt} -> createdAt) (\s@ListSolFunctionPackageMetadata' {} a -> s {createdAt = a} :: ListSolFunctionPackageMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
listSolFunctionPackageMetadata_lastModified :: Lens.Lens' ListSolFunctionPackageMetadata Prelude.UTCTime
listSolFunctionPackageMetadata_lastModified = Lens.lens (\ListSolFunctionPackageMetadata' {lastModified} -> lastModified) (\s@ListSolFunctionPackageMetadata' {} a -> s {lastModified = a} :: ListSolFunctionPackageMetadata) Prelude.. Data._Time

instance Data.FromJSON ListSolFunctionPackageMetadata where
  parseJSON =
    Data.withObject
      "ListSolFunctionPackageMetadata"
      ( \x ->
          ListSolFunctionPackageMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    ListSolFunctionPackageMetadata
  where
  hashWithSalt
    _salt
    ListSolFunctionPackageMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    ListSolFunctionPackageMetadata
  where
  rnf ListSolFunctionPackageMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
