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
-- Module      : Amazonka.TNB.Types.UpdateSolNetworkModify
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.UpdateSolNetworkModify where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.Document

-- | Information parameters and\/or the configurable properties for a network
-- function.
--
-- A network function instance is a function in a function package .
--
-- /See:/ 'newUpdateSolNetworkModify' smart constructor.
data UpdateSolNetworkModify = UpdateSolNetworkModify'
  { -- | Provides values for the configurable properties declared in the function
    -- package descriptor.
    vnfConfigurableProperties :: Document,
    -- | ID of the network function instance.
    --
    -- A network function instance is a function in a function package .
    vnfInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolNetworkModify' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfConfigurableProperties', 'updateSolNetworkModify_vnfConfigurableProperties' - Provides values for the configurable properties declared in the function
-- package descriptor.
--
-- 'vnfInstanceId', 'updateSolNetworkModify_vnfInstanceId' - ID of the network function instance.
--
-- A network function instance is a function in a function package .
newUpdateSolNetworkModify ::
  -- | 'vnfConfigurableProperties'
  Document ->
  -- | 'vnfInstanceId'
  Prelude.Text ->
  UpdateSolNetworkModify
newUpdateSolNetworkModify
  pVnfConfigurableProperties_
  pVnfInstanceId_ =
    UpdateSolNetworkModify'
      { vnfConfigurableProperties =
          pVnfConfigurableProperties_,
        vnfInstanceId = pVnfInstanceId_
      }

-- | Provides values for the configurable properties declared in the function
-- package descriptor.
updateSolNetworkModify_vnfConfigurableProperties :: Lens.Lens' UpdateSolNetworkModify Document
updateSolNetworkModify_vnfConfigurableProperties = Lens.lens (\UpdateSolNetworkModify' {vnfConfigurableProperties} -> vnfConfigurableProperties) (\s@UpdateSolNetworkModify' {} a -> s {vnfConfigurableProperties = a} :: UpdateSolNetworkModify)

-- | ID of the network function instance.
--
-- A network function instance is a function in a function package .
updateSolNetworkModify_vnfInstanceId :: Lens.Lens' UpdateSolNetworkModify Prelude.Text
updateSolNetworkModify_vnfInstanceId = Lens.lens (\UpdateSolNetworkModify' {vnfInstanceId} -> vnfInstanceId) (\s@UpdateSolNetworkModify' {} a -> s {vnfInstanceId = a} :: UpdateSolNetworkModify)

instance Prelude.Hashable UpdateSolNetworkModify where
  hashWithSalt _salt UpdateSolNetworkModify' {..} =
    _salt
      `Prelude.hashWithSalt` vnfConfigurableProperties
      `Prelude.hashWithSalt` vnfInstanceId

instance Prelude.NFData UpdateSolNetworkModify where
  rnf UpdateSolNetworkModify' {..} =
    Prelude.rnf vnfConfigurableProperties
      `Prelude.seq` Prelude.rnf vnfInstanceId

instance Data.ToJSON UpdateSolNetworkModify where
  toJSON UpdateSolNetworkModify' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "vnfConfigurableProperties"
                  Data..= vnfConfigurableProperties
              ),
            Prelude.Just
              ("vnfInstanceId" Data..= vnfInstanceId)
          ]
      )
