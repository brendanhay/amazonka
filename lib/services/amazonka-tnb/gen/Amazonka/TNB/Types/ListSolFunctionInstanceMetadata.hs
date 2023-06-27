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
-- Module      : Amazonka.TNB.Types.ListSolFunctionInstanceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolFunctionInstanceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lists network function instance metadata.
--
-- A network function instance is a function in a function package .
--
-- /See:/ 'newListSolFunctionInstanceMetadata' smart constructor.
data ListSolFunctionInstanceMetadata = ListSolFunctionInstanceMetadata'
  { -- | When the network function instance was created.
    createdAt :: Data.ISO8601,
    -- | When the network function instance was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionInstanceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listSolFunctionInstanceMetadata_createdAt' - When the network function instance was created.
--
-- 'lastModified', 'listSolFunctionInstanceMetadata_lastModified' - When the network function instance was last modified.
newListSolFunctionInstanceMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  ListSolFunctionInstanceMetadata
newListSolFunctionInstanceMetadata
  pCreatedAt_
  pLastModified_ =
    ListSolFunctionInstanceMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | When the network function instance was created.
listSolFunctionInstanceMetadata_createdAt :: Lens.Lens' ListSolFunctionInstanceMetadata Prelude.UTCTime
listSolFunctionInstanceMetadata_createdAt = Lens.lens (\ListSolFunctionInstanceMetadata' {createdAt} -> createdAt) (\s@ListSolFunctionInstanceMetadata' {} a -> s {createdAt = a} :: ListSolFunctionInstanceMetadata) Prelude.. Data._Time

-- | When the network function instance was last modified.
listSolFunctionInstanceMetadata_lastModified :: Lens.Lens' ListSolFunctionInstanceMetadata Prelude.UTCTime
listSolFunctionInstanceMetadata_lastModified = Lens.lens (\ListSolFunctionInstanceMetadata' {lastModified} -> lastModified) (\s@ListSolFunctionInstanceMetadata' {} a -> s {lastModified = a} :: ListSolFunctionInstanceMetadata) Prelude.. Data._Time

instance
  Data.FromJSON
    ListSolFunctionInstanceMetadata
  where
  parseJSON =
    Data.withObject
      "ListSolFunctionInstanceMetadata"
      ( \x ->
          ListSolFunctionInstanceMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    ListSolFunctionInstanceMetadata
  where
  hashWithSalt
    _salt
    ListSolFunctionInstanceMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    ListSolFunctionInstanceMetadata
  where
  rnf ListSolFunctionInstanceMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
