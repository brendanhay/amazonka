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
-- Module      : Amazonka.OpenSearchServerless.Types.VpcEndpointSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.VpcEndpointSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | The VPC endpoint object.
--
-- /See:/ 'newVpcEndpointSummary' smart constructor.
data VpcEndpointSummary = VpcEndpointSummary'
  { -- | The unique identifier of the endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the endpoint.
    status :: Prelude.Maybe VpcEndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'vpcEndpointSummary_id' - The unique identifier of the endpoint.
--
-- 'name', 'vpcEndpointSummary_name' - The name of the endpoint.
--
-- 'status', 'vpcEndpointSummary_status' - The current status of the endpoint.
newVpcEndpointSummary ::
  VpcEndpointSummary
newVpcEndpointSummary =
  VpcEndpointSummary'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique identifier of the endpoint.
vpcEndpointSummary_id :: Lens.Lens' VpcEndpointSummary (Prelude.Maybe Prelude.Text)
vpcEndpointSummary_id = Lens.lens (\VpcEndpointSummary' {id} -> id) (\s@VpcEndpointSummary' {} a -> s {id = a} :: VpcEndpointSummary)

-- | The name of the endpoint.
vpcEndpointSummary_name :: Lens.Lens' VpcEndpointSummary (Prelude.Maybe Prelude.Text)
vpcEndpointSummary_name = Lens.lens (\VpcEndpointSummary' {name} -> name) (\s@VpcEndpointSummary' {} a -> s {name = a} :: VpcEndpointSummary)

-- | The current status of the endpoint.
vpcEndpointSummary_status :: Lens.Lens' VpcEndpointSummary (Prelude.Maybe VpcEndpointStatus)
vpcEndpointSummary_status = Lens.lens (\VpcEndpointSummary' {status} -> status) (\s@VpcEndpointSummary' {} a -> s {status = a} :: VpcEndpointSummary)

instance Data.FromJSON VpcEndpointSummary where
  parseJSON =
    Data.withObject
      "VpcEndpointSummary"
      ( \x ->
          VpcEndpointSummary'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable VpcEndpointSummary where
  hashWithSalt _salt VpcEndpointSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData VpcEndpointSummary where
  rnf VpcEndpointSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
