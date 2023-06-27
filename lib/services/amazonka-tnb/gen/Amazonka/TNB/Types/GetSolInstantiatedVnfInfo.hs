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
-- Module      : Amazonka.TNB.Types.GetSolInstantiatedVnfInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolInstantiatedVnfInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.VnfOperationalState

-- | Information about a network function.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newGetSolInstantiatedVnfInfo' smart constructor.
data GetSolInstantiatedVnfInfo = GetSolInstantiatedVnfInfo'
  { -- | State of the network function.
    vnfState :: Prelude.Maybe VnfOperationalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolInstantiatedVnfInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfState', 'getSolInstantiatedVnfInfo_vnfState' - State of the network function.
newGetSolInstantiatedVnfInfo ::
  GetSolInstantiatedVnfInfo
newGetSolInstantiatedVnfInfo =
  GetSolInstantiatedVnfInfo'
    { vnfState =
        Prelude.Nothing
    }

-- | State of the network function.
getSolInstantiatedVnfInfo_vnfState :: Lens.Lens' GetSolInstantiatedVnfInfo (Prelude.Maybe VnfOperationalState)
getSolInstantiatedVnfInfo_vnfState = Lens.lens (\GetSolInstantiatedVnfInfo' {vnfState} -> vnfState) (\s@GetSolInstantiatedVnfInfo' {} a -> s {vnfState = a} :: GetSolInstantiatedVnfInfo)

instance Data.FromJSON GetSolInstantiatedVnfInfo where
  parseJSON =
    Data.withObject
      "GetSolInstantiatedVnfInfo"
      ( \x ->
          GetSolInstantiatedVnfInfo'
            Prelude.<$> (x Data..:? "vnfState")
      )

instance Prelude.Hashable GetSolInstantiatedVnfInfo where
  hashWithSalt _salt GetSolInstantiatedVnfInfo' {..} =
    _salt `Prelude.hashWithSalt` vnfState

instance Prelude.NFData GetSolInstantiatedVnfInfo where
  rnf GetSolInstantiatedVnfInfo' {..} =
    Prelude.rnf vnfState
