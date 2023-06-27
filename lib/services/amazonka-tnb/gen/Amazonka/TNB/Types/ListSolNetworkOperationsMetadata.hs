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
-- Module      : Amazonka.TNB.Types.ListSolNetworkOperationsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkOperationsMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata related to a network operation.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
--
-- /See:/ 'newListSolNetworkOperationsMetadata' smart constructor.
data ListSolNetworkOperationsMetadata = ListSolNetworkOperationsMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkOperationsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listSolNetworkOperationsMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'listSolNetworkOperationsMetadata_lastModified' - The date that the resource was last modified.
newListSolNetworkOperationsMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  ListSolNetworkOperationsMetadata
newListSolNetworkOperationsMetadata
  pCreatedAt_
  pLastModified_ =
    ListSolNetworkOperationsMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
listSolNetworkOperationsMetadata_createdAt :: Lens.Lens' ListSolNetworkOperationsMetadata Prelude.UTCTime
listSolNetworkOperationsMetadata_createdAt = Lens.lens (\ListSolNetworkOperationsMetadata' {createdAt} -> createdAt) (\s@ListSolNetworkOperationsMetadata' {} a -> s {createdAt = a} :: ListSolNetworkOperationsMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
listSolNetworkOperationsMetadata_lastModified :: Lens.Lens' ListSolNetworkOperationsMetadata Prelude.UTCTime
listSolNetworkOperationsMetadata_lastModified = Lens.lens (\ListSolNetworkOperationsMetadata' {lastModified} -> lastModified) (\s@ListSolNetworkOperationsMetadata' {} a -> s {lastModified = a} :: ListSolNetworkOperationsMetadata) Prelude.. Data._Time

instance
  Data.FromJSON
    ListSolNetworkOperationsMetadata
  where
  parseJSON =
    Data.withObject
      "ListSolNetworkOperationsMetadata"
      ( \x ->
          ListSolNetworkOperationsMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    ListSolNetworkOperationsMetadata
  where
  hashWithSalt
    _salt
    ListSolNetworkOperationsMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    ListSolNetworkOperationsMetadata
  where
  rnf ListSolNetworkOperationsMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
