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
-- Module      : Amazonka.TNB.Types.GetSolFunctionInstanceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolFunctionInstanceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a network function instance.
--
-- A network function instance is a function in a function package .
--
-- /See:/ 'newGetSolFunctionInstanceMetadata' smart constructor.
data GetSolFunctionInstanceMetadata = GetSolFunctionInstanceMetadata'
  { -- | The date that the resource was created.
    createdAt :: Data.ISO8601,
    -- | The date that the resource was last modified.
    lastModified :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionInstanceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getSolFunctionInstanceMetadata_createdAt' - The date that the resource was created.
--
-- 'lastModified', 'getSolFunctionInstanceMetadata_lastModified' - The date that the resource was last modified.
newGetSolFunctionInstanceMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  GetSolFunctionInstanceMetadata
newGetSolFunctionInstanceMetadata
  pCreatedAt_
  pLastModified_ =
    GetSolFunctionInstanceMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModified =
          Data._Time Lens.# pLastModified_
      }

-- | The date that the resource was created.
getSolFunctionInstanceMetadata_createdAt :: Lens.Lens' GetSolFunctionInstanceMetadata Prelude.UTCTime
getSolFunctionInstanceMetadata_createdAt = Lens.lens (\GetSolFunctionInstanceMetadata' {createdAt} -> createdAt) (\s@GetSolFunctionInstanceMetadata' {} a -> s {createdAt = a} :: GetSolFunctionInstanceMetadata) Prelude.. Data._Time

-- | The date that the resource was last modified.
getSolFunctionInstanceMetadata_lastModified :: Lens.Lens' GetSolFunctionInstanceMetadata Prelude.UTCTime
getSolFunctionInstanceMetadata_lastModified = Lens.lens (\GetSolFunctionInstanceMetadata' {lastModified} -> lastModified) (\s@GetSolFunctionInstanceMetadata' {} a -> s {lastModified = a} :: GetSolFunctionInstanceMetadata) Prelude.. Data._Time

instance Data.FromJSON GetSolFunctionInstanceMetadata where
  parseJSON =
    Data.withObject
      "GetSolFunctionInstanceMetadata"
      ( \x ->
          GetSolFunctionInstanceMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModified")
      )

instance
  Prelude.Hashable
    GetSolFunctionInstanceMetadata
  where
  hashWithSalt
    _salt
    GetSolFunctionInstanceMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastModified

instance
  Prelude.NFData
    GetSolFunctionInstanceMetadata
  where
  rnf GetSolFunctionInstanceMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModified
