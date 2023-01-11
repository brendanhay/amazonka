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
-- Module      : Amazonka.OpenSearchServerless.Types.VpcEndpointFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.VpcEndpointFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Filter the results of a @ListVpcEndpoints@ request.
--
-- /See:/ 'newVpcEndpointFilters' smart constructor.
data VpcEndpointFilters = VpcEndpointFilters'
  { -- | The current status of the endpoint.
    status :: Prelude.Maybe VpcEndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'vpcEndpointFilters_status' - The current status of the endpoint.
newVpcEndpointFilters ::
  VpcEndpointFilters
newVpcEndpointFilters =
  VpcEndpointFilters' {status = Prelude.Nothing}

-- | The current status of the endpoint.
vpcEndpointFilters_status :: Lens.Lens' VpcEndpointFilters (Prelude.Maybe VpcEndpointStatus)
vpcEndpointFilters_status = Lens.lens (\VpcEndpointFilters' {status} -> status) (\s@VpcEndpointFilters' {} a -> s {status = a} :: VpcEndpointFilters)

instance Prelude.Hashable VpcEndpointFilters where
  hashWithSalt _salt VpcEndpointFilters' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData VpcEndpointFilters where
  rnf VpcEndpointFilters' {..} = Prelude.rnf status

instance Data.ToJSON VpcEndpointFilters where
  toJSON VpcEndpointFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("status" Data..=) Prelude.<$> status]
      )
