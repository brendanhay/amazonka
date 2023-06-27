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
-- Module      : Amazonka.TNB.Types.GetSolVnfcResourceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolVnfcResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.GetSolVnfcResourceInfoMetadata

-- | Details of resource associated with a network function.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newGetSolVnfcResourceInfo' smart constructor.
data GetSolVnfcResourceInfo = GetSolVnfcResourceInfo'
  { -- | The metadata of the network function compute.
    metadata :: Prelude.Maybe GetSolVnfcResourceInfoMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolVnfcResourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getSolVnfcResourceInfo_metadata' - The metadata of the network function compute.
newGetSolVnfcResourceInfo ::
  GetSolVnfcResourceInfo
newGetSolVnfcResourceInfo =
  GetSolVnfcResourceInfo' {metadata = Prelude.Nothing}

-- | The metadata of the network function compute.
getSolVnfcResourceInfo_metadata :: Lens.Lens' GetSolVnfcResourceInfo (Prelude.Maybe GetSolVnfcResourceInfoMetadata)
getSolVnfcResourceInfo_metadata = Lens.lens (\GetSolVnfcResourceInfo' {metadata} -> metadata) (\s@GetSolVnfcResourceInfo' {} a -> s {metadata = a} :: GetSolVnfcResourceInfo)

instance Data.FromJSON GetSolVnfcResourceInfo where
  parseJSON =
    Data.withObject
      "GetSolVnfcResourceInfo"
      ( \x ->
          GetSolVnfcResourceInfo'
            Prelude.<$> (x Data..:? "metadata")
      )

instance Prelude.Hashable GetSolVnfcResourceInfo where
  hashWithSalt _salt GetSolVnfcResourceInfo' {..} =
    _salt `Prelude.hashWithSalt` metadata

instance Prelude.NFData GetSolVnfcResourceInfo where
  rnf GetSolVnfcResourceInfo' {..} =
    Prelude.rnf metadata
