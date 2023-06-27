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
-- Module      : Amazonka.Route53.Types.ReusableDelegationSetLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.ReusableDelegationSetLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.ReusableDelegationSetLimitType

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

instance Data.FromXML ReusableDelegationSetLimit where
  parseXML x =
    ReusableDelegationSetLimit'
      Prelude.<$> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "Value")

instance Prelude.Hashable ReusableDelegationSetLimit where
  hashWithSalt _salt ReusableDelegationSetLimit' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReusableDelegationSetLimit where
  rnf ReusableDelegationSetLimit' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value
