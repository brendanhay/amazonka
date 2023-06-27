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
-- Module      : Amazonka.SecurityHub.Types.PropagatingVgwSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PropagatingVgwSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a virtual private gateway propagating route.
--
-- /See:/ 'newPropagatingVgwSetDetails' smart constructor.
data PropagatingVgwSetDetails = PropagatingVgwSetDetails'
  { -- | The ID of the virtual private gateway.
    gatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropagatingVgwSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'propagatingVgwSetDetails_gatewayId' - The ID of the virtual private gateway.
newPropagatingVgwSetDetails ::
  PropagatingVgwSetDetails
newPropagatingVgwSetDetails =
  PropagatingVgwSetDetails'
    { gatewayId =
        Prelude.Nothing
    }

-- | The ID of the virtual private gateway.
propagatingVgwSetDetails_gatewayId :: Lens.Lens' PropagatingVgwSetDetails (Prelude.Maybe Prelude.Text)
propagatingVgwSetDetails_gatewayId = Lens.lens (\PropagatingVgwSetDetails' {gatewayId} -> gatewayId) (\s@PropagatingVgwSetDetails' {} a -> s {gatewayId = a} :: PropagatingVgwSetDetails)

instance Data.FromJSON PropagatingVgwSetDetails where
  parseJSON =
    Data.withObject
      "PropagatingVgwSetDetails"
      ( \x ->
          PropagatingVgwSetDetails'
            Prelude.<$> (x Data..:? "GatewayId")
      )

instance Prelude.Hashable PropagatingVgwSetDetails where
  hashWithSalt _salt PropagatingVgwSetDetails' {..} =
    _salt `Prelude.hashWithSalt` gatewayId

instance Prelude.NFData PropagatingVgwSetDetails where
  rnf PropagatingVgwSetDetails' {..} =
    Prelude.rnf gatewayId

instance Data.ToJSON PropagatingVgwSetDetails where
  toJSON PropagatingVgwSetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("GatewayId" Data..=) Prelude.<$> gatewayId]
      )
