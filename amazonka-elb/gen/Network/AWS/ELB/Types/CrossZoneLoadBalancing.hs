{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELB.Types.CrossZoneLoadBalancing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.CrossZoneLoadBalancing where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
-- /See:/ 'newCrossZoneLoadBalancing' smart constructor.
data CrossZoneLoadBalancing = CrossZoneLoadBalancing'
  { -- | Specifies whether cross-zone load balancing is enabled for the load
    -- balancer.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML CrossZoneLoadBalancing where
  parseXML x =
    CrossZoneLoadBalancing'
      Prelude.<$> (x Prelude..@ "Enabled")

instance Prelude.Hashable CrossZoneLoadBalancing

instance Prelude.NFData CrossZoneLoadBalancing

instance Prelude.ToQuery CrossZoneLoadBalancing where
  toQuery CrossZoneLoadBalancing' {..} =
    Prelude.mconcat ["Enabled" Prelude.=: enabled]
