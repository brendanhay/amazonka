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
-- Module      : Network.AWS.DynamoDB.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Endpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An endpoint information details.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | IP address of the endpoint.
    address :: Core.Text,
    -- | Endpoint cache time to live (TTL) value.
    cachePeriodInMinutes :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpoint_address' - IP address of the endpoint.
--
-- 'cachePeriodInMinutes', 'endpoint_cachePeriodInMinutes' - Endpoint cache time to live (TTL) value.
newEndpoint ::
  -- | 'address'
  Core.Text ->
  -- | 'cachePeriodInMinutes'
  Core.Integer ->
  Endpoint
newEndpoint pAddress_ pCachePeriodInMinutes_ =
  Endpoint'
    { address = pAddress_,
      cachePeriodInMinutes = pCachePeriodInMinutes_
    }

-- | IP address of the endpoint.
endpoint_address :: Lens.Lens' Endpoint Core.Text
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | Endpoint cache time to live (TTL) value.
endpoint_cachePeriodInMinutes :: Lens.Lens' Endpoint Core.Integer
endpoint_cachePeriodInMinutes = Lens.lens (\Endpoint' {cachePeriodInMinutes} -> cachePeriodInMinutes) (\s@Endpoint' {} a -> s {cachePeriodInMinutes = a} :: Endpoint)

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Core.<$> (x Core..: "Address")
            Core.<*> (x Core..: "CachePeriodInMinutes")
      )

instance Core.Hashable Endpoint

instance Core.NFData Endpoint
