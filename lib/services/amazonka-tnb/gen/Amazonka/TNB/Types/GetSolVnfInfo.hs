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
-- Module      : Amazonka.TNB.Types.GetSolVnfInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolVnfInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.GetSolVnfcResourceInfo
import Amazonka.TNB.Types.VnfOperationalState

-- | Information about the network function.
--
-- A network function instance is a function in a function package .
--
-- /See:/ 'newGetSolVnfInfo' smart constructor.
data GetSolVnfInfo = GetSolVnfInfo'
  { -- | State of the network function instance.
    vnfState :: Prelude.Maybe VnfOperationalState,
    -- | Compute info used by the network function instance.
    vnfcResourceInfo :: Prelude.Maybe [GetSolVnfcResourceInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolVnfInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfState', 'getSolVnfInfo_vnfState' - State of the network function instance.
--
-- 'vnfcResourceInfo', 'getSolVnfInfo_vnfcResourceInfo' - Compute info used by the network function instance.
newGetSolVnfInfo ::
  GetSolVnfInfo
newGetSolVnfInfo =
  GetSolVnfInfo'
    { vnfState = Prelude.Nothing,
      vnfcResourceInfo = Prelude.Nothing
    }

-- | State of the network function instance.
getSolVnfInfo_vnfState :: Lens.Lens' GetSolVnfInfo (Prelude.Maybe VnfOperationalState)
getSolVnfInfo_vnfState = Lens.lens (\GetSolVnfInfo' {vnfState} -> vnfState) (\s@GetSolVnfInfo' {} a -> s {vnfState = a} :: GetSolVnfInfo)

-- | Compute info used by the network function instance.
getSolVnfInfo_vnfcResourceInfo :: Lens.Lens' GetSolVnfInfo (Prelude.Maybe [GetSolVnfcResourceInfo])
getSolVnfInfo_vnfcResourceInfo = Lens.lens (\GetSolVnfInfo' {vnfcResourceInfo} -> vnfcResourceInfo) (\s@GetSolVnfInfo' {} a -> s {vnfcResourceInfo = a} :: GetSolVnfInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GetSolVnfInfo where
  parseJSON =
    Data.withObject
      "GetSolVnfInfo"
      ( \x ->
          GetSolVnfInfo'
            Prelude.<$> (x Data..:? "vnfState")
            Prelude.<*> ( x
                            Data..:? "vnfcResourceInfo"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GetSolVnfInfo where
  hashWithSalt _salt GetSolVnfInfo' {..} =
    _salt
      `Prelude.hashWithSalt` vnfState
      `Prelude.hashWithSalt` vnfcResourceInfo

instance Prelude.NFData GetSolVnfInfo where
  rnf GetSolVnfInfo' {..} =
    Prelude.rnf vnfState
      `Prelude.seq` Prelude.rnf vnfcResourceInfo
