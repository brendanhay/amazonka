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
-- Module      : Amazonka.TimeStreamQuery.Types.Endpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an available endpoint against which to make API calls
-- against, as well as the TTL for that endpoint.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | An endpoint address.
    address :: Prelude.Text,
    -- | The TTL for the endpoint, in minutes.
    cachePeriodInMinutes :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpoint_address' - An endpoint address.
--
-- 'cachePeriodInMinutes', 'endpoint_cachePeriodInMinutes' - The TTL for the endpoint, in minutes.
newEndpoint ::
  -- | 'address'
  Prelude.Text ->
  -- | 'cachePeriodInMinutes'
  Prelude.Integer ->
  Endpoint
newEndpoint pAddress_ pCachePeriodInMinutes_ =
  Endpoint'
    { address = pAddress_,
      cachePeriodInMinutes = pCachePeriodInMinutes_
    }

-- | An endpoint address.
endpoint_address :: Lens.Lens' Endpoint Prelude.Text
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | The TTL for the endpoint, in minutes.
endpoint_cachePeriodInMinutes :: Lens.Lens' Endpoint Prelude.Integer
endpoint_cachePeriodInMinutes = Lens.lens (\Endpoint' {cachePeriodInMinutes} -> cachePeriodInMinutes) (\s@Endpoint' {} a -> s {cachePeriodInMinutes = a} :: Endpoint)

instance Data.FromJSON Endpoint where
  parseJSON =
    Data.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Data..: "Address")
            Prelude.<*> (x Data..: "CachePeriodInMinutes")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` cachePeriodInMinutes

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf cachePeriodInMinutes
