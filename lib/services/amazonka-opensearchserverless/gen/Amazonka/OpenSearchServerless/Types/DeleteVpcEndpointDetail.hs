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
-- Module      : Amazonka.OpenSearchServerless.Types.DeleteVpcEndpointDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.DeleteVpcEndpointDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Deletion details for an OpenSearch Serverless-managed interface
-- endpoint.
--
-- /See:/ 'newDeleteVpcEndpointDetail' smart constructor.
data DeleteVpcEndpointDetail = DeleteVpcEndpointDetail'
  { -- | The unique identifier of the endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the endpoint deletion process.
    status :: Prelude.Maybe VpcEndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteVpcEndpointDetail_id' - The unique identifier of the endpoint.
--
-- 'name', 'deleteVpcEndpointDetail_name' - The name of the endpoint.
--
-- 'status', 'deleteVpcEndpointDetail_status' - The current status of the endpoint deletion process.
newDeleteVpcEndpointDetail ::
  DeleteVpcEndpointDetail
newDeleteVpcEndpointDetail =
  DeleteVpcEndpointDetail'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique identifier of the endpoint.
deleteVpcEndpointDetail_id :: Lens.Lens' DeleteVpcEndpointDetail (Prelude.Maybe Prelude.Text)
deleteVpcEndpointDetail_id = Lens.lens (\DeleteVpcEndpointDetail' {id} -> id) (\s@DeleteVpcEndpointDetail' {} a -> s {id = a} :: DeleteVpcEndpointDetail)

-- | The name of the endpoint.
deleteVpcEndpointDetail_name :: Lens.Lens' DeleteVpcEndpointDetail (Prelude.Maybe Prelude.Text)
deleteVpcEndpointDetail_name = Lens.lens (\DeleteVpcEndpointDetail' {name} -> name) (\s@DeleteVpcEndpointDetail' {} a -> s {name = a} :: DeleteVpcEndpointDetail)

-- | The current status of the endpoint deletion process.
deleteVpcEndpointDetail_status :: Lens.Lens' DeleteVpcEndpointDetail (Prelude.Maybe VpcEndpointStatus)
deleteVpcEndpointDetail_status = Lens.lens (\DeleteVpcEndpointDetail' {status} -> status) (\s@DeleteVpcEndpointDetail' {} a -> s {status = a} :: DeleteVpcEndpointDetail)

instance Data.FromJSON DeleteVpcEndpointDetail where
  parseJSON =
    Data.withObject
      "DeleteVpcEndpointDetail"
      ( \x ->
          DeleteVpcEndpointDetail'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DeleteVpcEndpointDetail where
  hashWithSalt _salt DeleteVpcEndpointDetail' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DeleteVpcEndpointDetail where
  rnf DeleteVpcEndpointDetail' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
