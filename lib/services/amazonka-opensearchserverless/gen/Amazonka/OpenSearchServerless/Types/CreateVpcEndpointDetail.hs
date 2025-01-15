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
-- Module      : Amazonka.OpenSearchServerless.Types.CreateVpcEndpointDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CreateVpcEndpointDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Creation details for an OpenSearch Serverless-managed interface
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
--
-- /See:/ 'newCreateVpcEndpointDetail' smart constructor.
data CreateVpcEndpointDetail = CreateVpcEndpointDetail'
  { -- | The unique identifier of the endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status in the endpoint creation process.
    status :: Prelude.Maybe VpcEndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createVpcEndpointDetail_id' - The unique identifier of the endpoint.
--
-- 'name', 'createVpcEndpointDetail_name' - The name of the endpoint.
--
-- 'status', 'createVpcEndpointDetail_status' - The current status in the endpoint creation process.
newCreateVpcEndpointDetail ::
  CreateVpcEndpointDetail
newCreateVpcEndpointDetail =
  CreateVpcEndpointDetail'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique identifier of the endpoint.
createVpcEndpointDetail_id :: Lens.Lens' CreateVpcEndpointDetail (Prelude.Maybe Prelude.Text)
createVpcEndpointDetail_id = Lens.lens (\CreateVpcEndpointDetail' {id} -> id) (\s@CreateVpcEndpointDetail' {} a -> s {id = a} :: CreateVpcEndpointDetail)

-- | The name of the endpoint.
createVpcEndpointDetail_name :: Lens.Lens' CreateVpcEndpointDetail (Prelude.Maybe Prelude.Text)
createVpcEndpointDetail_name = Lens.lens (\CreateVpcEndpointDetail' {name} -> name) (\s@CreateVpcEndpointDetail' {} a -> s {name = a} :: CreateVpcEndpointDetail)

-- | The current status in the endpoint creation process.
createVpcEndpointDetail_status :: Lens.Lens' CreateVpcEndpointDetail (Prelude.Maybe VpcEndpointStatus)
createVpcEndpointDetail_status = Lens.lens (\CreateVpcEndpointDetail' {status} -> status) (\s@CreateVpcEndpointDetail' {} a -> s {status = a} :: CreateVpcEndpointDetail)

instance Data.FromJSON CreateVpcEndpointDetail where
  parseJSON =
    Data.withObject
      "CreateVpcEndpointDetail"
      ( \x ->
          CreateVpcEndpointDetail'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable CreateVpcEndpointDetail where
  hashWithSalt _salt CreateVpcEndpointDetail' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData CreateVpcEndpointDetail where
  rnf CreateVpcEndpointDetail' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf status
