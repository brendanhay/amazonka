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
-- Module      : Amazonka.TNB.Types.ListSolNetworkInstanceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkInstanceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata details for a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newListSolNetworkInstanceMetadata' smart constructor.
data ListSolNetworkInstanceMetadata = ListSolNetworkInstanceMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkInstanceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listSolNetworkInstanceMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'listSolNetworkInstanceMetadata_lastModified' - The date that the resource was last modified.
newListSolNetworkInstanceMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  ListSolNetworkInstanceMetadata
newListSolNetworkInstanceMetadata
  pCreatedAt_
  pLastModified_ =
    ListSolNetworkInstanceMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
listSolNetworkInstanceMetadata_createdAt :: Lens.Lens' ListSolNetworkInstanceMetadata Prelude.UTCTime
listSolNetworkInstanceMetadata_createdAt = Lens.lens (\ListSolNetworkInstanceMetadata' {createdAt} -> createdAt) (\s@ListSolNetworkInstanceMetadata' {} a -> s {createdAt = a} :: ListSolNetworkInstanceMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
listSolNetworkInstanceMetadata_lastModified :: Lens.Lens' ListSolNetworkInstanceMetadata Prelude.UTCTime
listSolNetworkInstanceMetadata_lastModified = Lens.lens (\ListSolNetworkInstanceMetadata' {lastModified} -> lastModified) (\s@ListSolNetworkInstanceMetadata' {} a -> s {lastModified = a} :: ListSolNetworkInstanceMetadata) Prelude.. Data._Time

instance Data.FromJSON ListSolNetworkInstanceMetadata where
  parseJSON =
    Data.withObject
      "ListSolNetworkInstanceMetadata"
      ( \x ->
          ListSolNetworkInstanceMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    ListSolNetworkInstanceMetadata
  where
  hashWithSalt
    _salt
    ListSolNetworkInstanceMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    ListSolNetworkInstanceMetadata
  where
  rnf ListSolNetworkInstanceMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
