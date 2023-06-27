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
-- Module      : Amazonka.TNB.Types.GetSolNetworkInstanceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolNetworkInstanceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newGetSolNetworkInstanceMetadata' smart constructor.
data GetSolNetworkInstanceMetadata = GetSolNetworkInstanceMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkInstanceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getSolNetworkInstanceMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'getSolNetworkInstanceMetadata_lastModified' - The date that the resource was last modified.
newGetSolNetworkInstanceMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  GetSolNetworkInstanceMetadata
newGetSolNetworkInstanceMetadata
  pCreatedAt_
  pLastModified_ =
    GetSolNetworkInstanceMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
getSolNetworkInstanceMetadata_createdAt :: Lens.Lens' GetSolNetworkInstanceMetadata Prelude.UTCTime
getSolNetworkInstanceMetadata_createdAt = Lens.lens (\GetSolNetworkInstanceMetadata' {createdAt} -> createdAt) (\s@GetSolNetworkInstanceMetadata' {} a -> s {createdAt = a} :: GetSolNetworkInstanceMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
getSolNetworkInstanceMetadata_lastModified :: Lens.Lens' GetSolNetworkInstanceMetadata Prelude.UTCTime
getSolNetworkInstanceMetadata_lastModified = Lens.lens (\GetSolNetworkInstanceMetadata' {lastModified} -> lastModified) (\s@GetSolNetworkInstanceMetadata' {} a -> s {lastModified = a} :: GetSolNetworkInstanceMetadata) Prelude.. Data._Time

instance Data.FromJSON GetSolNetworkInstanceMetadata where
  parseJSON =
    Data.withObject
      "GetSolNetworkInstanceMetadata"
      ( \x ->
          GetSolNetworkInstanceMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    GetSolNetworkInstanceMetadata
  where
  hashWithSalt _salt GetSolNetworkInstanceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData GetSolNetworkInstanceMetadata where
  rnf GetSolNetworkInstanceMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
