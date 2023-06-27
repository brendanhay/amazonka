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
-- Module      : Amazonka.TNB.Types.ListSolNetworkPackageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkPackageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata related to a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- /See:/ 'newListSolNetworkPackageMetadata' smart constructor.
data ListSolNetworkPackageMetadata = ListSolNetworkPackageMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkPackageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listSolNetworkPackageMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'listSolNetworkPackageMetadata_lastModified' - The date that the resource was last modified.
newListSolNetworkPackageMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  ListSolNetworkPackageMetadata
newListSolNetworkPackageMetadata
  pCreatedAt_
  pLastModified_ =
    ListSolNetworkPackageMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
listSolNetworkPackageMetadata_createdAt :: Lens.Lens' ListSolNetworkPackageMetadata Prelude.UTCTime
listSolNetworkPackageMetadata_createdAt = Lens.lens (\ListSolNetworkPackageMetadata' {createdAt} -> createdAt) (\s@ListSolNetworkPackageMetadata' {} a -> s {createdAt = a} :: ListSolNetworkPackageMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
listSolNetworkPackageMetadata_lastModified :: Lens.Lens' ListSolNetworkPackageMetadata Prelude.UTCTime
listSolNetworkPackageMetadata_lastModified = Lens.lens (\ListSolNetworkPackageMetadata' {lastModified} -> lastModified) (\s@ListSolNetworkPackageMetadata' {} a -> s {lastModified = a} :: ListSolNetworkPackageMetadata) Prelude.. Data._Time

instance Data.FromJSON ListSolNetworkPackageMetadata where
  parseJSON =
    Data.withObject
      "ListSolNetworkPackageMetadata"
      ( \x ->
          ListSolNetworkPackageMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    ListSolNetworkPackageMetadata
  where
  hashWithSalt _salt ListSolNetworkPackageMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData ListSolNetworkPackageMetadata where
  rnf ListSolNetworkPackageMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
