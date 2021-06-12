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
-- Module      : Network.AWS.Route53.Types.HostedZoneLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneLimitType

-- | A complex type that contains the type of limit that you specified in the
-- request and the current value for that limit.
--
-- /See:/ 'newHostedZoneLimit' smart constructor.
data HostedZoneLimit = HostedZoneLimit'
  { -- | The limit that you requested. Valid values include the following:
    --
    -- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
    --     create in the specified hosted zone.
    --
    -- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
    --     that you can associate with the specified private hosted zone.
    type' :: HostedZoneLimitType,
    -- | The current value for the limit that is specified by @Type@.
    value :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HostedZoneLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'hostedZoneLimit_type' - The limit that you requested. Valid values include the following:
--
-- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
--     create in the specified hosted zone.
--
-- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
--     that you can associate with the specified private hosted zone.
--
-- 'value', 'hostedZoneLimit_value' - The current value for the limit that is specified by @Type@.
newHostedZoneLimit ::
  -- | 'type''
  HostedZoneLimitType ->
  -- | 'value'
  Core.Natural ->
  HostedZoneLimit
newHostedZoneLimit pType_ pValue_ =
  HostedZoneLimit' {type' = pType_, value = pValue_}

-- | The limit that you requested. Valid values include the following:
--
-- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
--     create in the specified hosted zone.
--
-- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
--     that you can associate with the specified private hosted zone.
hostedZoneLimit_type :: Lens.Lens' HostedZoneLimit HostedZoneLimitType
hostedZoneLimit_type = Lens.lens (\HostedZoneLimit' {type'} -> type') (\s@HostedZoneLimit' {} a -> s {type' = a} :: HostedZoneLimit)

-- | The current value for the limit that is specified by @Type@.
hostedZoneLimit_value :: Lens.Lens' HostedZoneLimit Core.Natural
hostedZoneLimit_value = Lens.lens (\HostedZoneLimit' {value} -> value) (\s@HostedZoneLimit' {} a -> s {value = a} :: HostedZoneLimit)

instance Core.FromXML HostedZoneLimit where
  parseXML x =
    HostedZoneLimit'
      Core.<$> (x Core..@ "Type") Core.<*> (x Core..@ "Value")

instance Core.Hashable HostedZoneLimit

instance Core.NFData HostedZoneLimit
