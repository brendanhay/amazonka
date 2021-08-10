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
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ReusableDelegationSetLimitType

-- | A complex type that contains the type of limit that you specified in the
-- request and the current value for that limit.
--
-- /See:/ 'newReusableDelegationSetLimit' smart constructor.
data ReusableDelegationSetLimit = ReusableDelegationSetLimit'
  { -- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@,
    -- the maximum number of hosted zones that you can associate with the
    -- specified reusable delegation set.
    type' :: ReusableDelegationSetLimitType,
    -- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
    value :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReusableDelegationSetLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'reusableDelegationSetLimit_type' - The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@,
-- the maximum number of hosted zones that you can associate with the
-- specified reusable delegation set.
--
-- 'value', 'reusableDelegationSetLimit_value' - The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
newReusableDelegationSetLimit ::
  -- | 'type''
  ReusableDelegationSetLimitType ->
  -- | 'value'
  Prelude.Natural ->
  ReusableDelegationSetLimit
newReusableDelegationSetLimit pType_ pValue_ =
  ReusableDelegationSetLimit'
    { type' = pType_,
      value = pValue_
    }

-- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@,
-- the maximum number of hosted zones that you can associate with the
-- specified reusable delegation set.
reusableDelegationSetLimit_type :: Lens.Lens' ReusableDelegationSetLimit ReusableDelegationSetLimitType
reusableDelegationSetLimit_type = Lens.lens (\ReusableDelegationSetLimit' {type'} -> type') (\s@ReusableDelegationSetLimit' {} a -> s {type' = a} :: ReusableDelegationSetLimit)

-- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
reusableDelegationSetLimit_value :: Lens.Lens' ReusableDelegationSetLimit Prelude.Natural
reusableDelegationSetLimit_value = Lens.lens (\ReusableDelegationSetLimit' {value} -> value) (\s@ReusableDelegationSetLimit' {} a -> s {value = a} :: ReusableDelegationSetLimit)

instance Core.FromXML ReusableDelegationSetLimit where
  parseXML x =
    ReusableDelegationSetLimit'
      Prelude.<$> (x Core..@ "Type") Prelude.<*> (x Core..@ "Value")

instance Prelude.Hashable ReusableDelegationSetLimit

instance Prelude.NFData ReusableDelegationSetLimit
