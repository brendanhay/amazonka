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
-- Module      : Amazonka.ELB.Types.CrossZoneLoadBalancing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.CrossZoneLoadBalancing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
-- /See:/ 'newCrossZoneLoadBalancing' smart constructor.
data CrossZoneLoadBalancing = CrossZoneLoadBalancing'
  { -- | Specifies whether cross-zone load balancing is enabled for the load
    -- balancer.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossZoneLoadBalancing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'crossZoneLoadBalancing_enabled' - Specifies whether cross-zone load balancing is enabled for the load
-- balancer.
newCrossZoneLoadBalancing ::
  -- | 'enabled'
  Prelude.Bool ->
  CrossZoneLoadBalancing
newCrossZoneLoadBalancing pEnabled_ =
  CrossZoneLoadBalancing' {enabled = pEnabled_}

-- | Specifies whether cross-zone load balancing is enabled for the load
-- balancer.
crossZoneLoadBalancing_enabled :: Lens.Lens' CrossZoneLoadBalancing Prelude.Bool
crossZoneLoadBalancing_enabled = Lens.lens (\CrossZoneLoadBalancing' {enabled} -> enabled) (\s@CrossZoneLoadBalancing' {} a -> s {enabled = a} :: CrossZoneLoadBalancing)

instance Data.FromXML CrossZoneLoadBalancing where
  parseXML x =
    CrossZoneLoadBalancing'
      Prelude.<$> (x Data..@ "Enabled")

instance Prelude.Hashable CrossZoneLoadBalancing where
  hashWithSalt _salt CrossZoneLoadBalancing' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData CrossZoneLoadBalancing where
  rnf CrossZoneLoadBalancing' {..} = Prelude.rnf enabled

instance Data.ToQuery CrossZoneLoadBalancing where
  toQuery CrossZoneLoadBalancing' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
