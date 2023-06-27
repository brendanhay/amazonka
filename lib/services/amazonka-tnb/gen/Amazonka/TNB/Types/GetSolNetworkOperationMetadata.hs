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
-- Module      : Amazonka.TNB.Types.GetSolNetworkOperationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolNetworkOperationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata related to a network operation occurrence.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
--
-- /See:/ 'newGetSolNetworkOperationMetadata' smart constructor.
data GetSolNetworkOperationMetadata = GetSolNetworkOperationMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkOperationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getSolNetworkOperationMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'getSolNetworkOperationMetadata_lastModified' - The date that the resource was last modified.
newGetSolNetworkOperationMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  GetSolNetworkOperationMetadata
newGetSolNetworkOperationMetadata
  pCreatedAt_
  pLastModified_ =
    GetSolNetworkOperationMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
getSolNetworkOperationMetadata_createdAt :: Lens.Lens' GetSolNetworkOperationMetadata Prelude.UTCTime
getSolNetworkOperationMetadata_createdAt = Lens.lens (\GetSolNetworkOperationMetadata' {createdAt} -> createdAt) (\s@GetSolNetworkOperationMetadata' {} a -> s {createdAt = a} :: GetSolNetworkOperationMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
getSolNetworkOperationMetadata_lastModified :: Lens.Lens' GetSolNetworkOperationMetadata Prelude.UTCTime
getSolNetworkOperationMetadata_lastModified = Lens.lens (\GetSolNetworkOperationMetadata' {lastModified} -> lastModified) (\s@GetSolNetworkOperationMetadata' {} a -> s {lastModified = a} :: GetSolNetworkOperationMetadata) Prelude.. Data._Time

instance Data.FromJSON GetSolNetworkOperationMetadata where
  parseJSON =
    Data.withObject
      "GetSolNetworkOperationMetadata"
      ( \x ->
          GetSolNetworkOperationMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    GetSolNetworkOperationMetadata
  where
  hashWithSalt
    _salt
    GetSolNetworkOperationMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    GetSolNetworkOperationMetadata
  where
  rnf GetSolNetworkOperationMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
