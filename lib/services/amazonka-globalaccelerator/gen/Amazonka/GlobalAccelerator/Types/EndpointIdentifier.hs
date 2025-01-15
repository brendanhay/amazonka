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
-- Module      : Amazonka.GlobalAccelerator.Types.EndpointIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.EndpointIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type for an endpoint. Specifies information about the endpoint
-- to remove from the endpoint group.
--
-- /See:/ 'newEndpointIdentifier' smart constructor.
data EndpointIdentifier = EndpointIdentifier'
  { -- | Indicates whether client IP address preservation is enabled for an
    -- endpoint. The value is true or false.
    --
    -- If the value is set to true, the client\'s IP address is preserved in
    -- the @X-Forwarded-For@ request header as traffic travels to applications
    -- on the endpoint fronted by the accelerator.
    clientIPPreservationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
    -- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
    -- resource. If the endpoint is an Elastic IP address, this is the Elastic
    -- IP address allocation ID. For Amazon EC2 instances, this is the EC2
    -- instance ID.
    --
    -- An Application Load Balancer can be either internal or internet-facing.
    endpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIPPreservationEnabled', 'endpointIdentifier_clientIPPreservationEnabled' - Indicates whether client IP address preservation is enabled for an
-- endpoint. The value is true or false.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the endpoint fronted by the accelerator.
--
-- 'endpointId', 'endpointIdentifier_endpointId' - An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
newEndpointIdentifier ::
  -- | 'endpointId'
  Prelude.Text ->
  EndpointIdentifier
newEndpointIdentifier pEndpointId_ =
  EndpointIdentifier'
    { clientIPPreservationEnabled =
        Prelude.Nothing,
      endpointId = pEndpointId_
    }

-- | Indicates whether client IP address preservation is enabled for an
-- endpoint. The value is true or false.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the endpoint fronted by the accelerator.
endpointIdentifier_clientIPPreservationEnabled :: Lens.Lens' EndpointIdentifier (Prelude.Maybe Prelude.Bool)
endpointIdentifier_clientIPPreservationEnabled = Lens.lens (\EndpointIdentifier' {clientIPPreservationEnabled} -> clientIPPreservationEnabled) (\s@EndpointIdentifier' {} a -> s {clientIPPreservationEnabled = a} :: EndpointIdentifier)

-- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
endpointIdentifier_endpointId :: Lens.Lens' EndpointIdentifier Prelude.Text
endpointIdentifier_endpointId = Lens.lens (\EndpointIdentifier' {endpointId} -> endpointId) (\s@EndpointIdentifier' {} a -> s {endpointId = a} :: EndpointIdentifier)

instance Prelude.Hashable EndpointIdentifier where
  hashWithSalt _salt EndpointIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` clientIPPreservationEnabled
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData EndpointIdentifier where
  rnf EndpointIdentifier' {..} =
    Prelude.rnf clientIPPreservationEnabled `Prelude.seq`
      Prelude.rnf endpointId

instance Data.ToJSON EndpointIdentifier where
  toJSON EndpointIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientIPPreservationEnabled" Data..=)
              Prelude.<$> clientIPPreservationEnabled,
            Prelude.Just ("EndpointId" Data..= endpointId)
          ]
      )
