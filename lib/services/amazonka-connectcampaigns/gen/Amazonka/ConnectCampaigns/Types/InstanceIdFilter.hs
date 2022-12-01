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
-- Module      : Amazonka.ConnectCampaigns.Types.InstanceIdFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.InstanceIdFilter where

import Amazonka.ConnectCampaigns.Types.InstanceIdFilterOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Connect instance identifier filter
--
-- /See:/ 'newInstanceIdFilter' smart constructor.
data InstanceIdFilter = InstanceIdFilter'
  { operator :: InstanceIdFilterOperator,
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIdFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'instanceIdFilter_operator' - Undocumented member.
--
-- 'value', 'instanceIdFilter_value' - Undocumented member.
newInstanceIdFilter ::
  -- | 'operator'
  InstanceIdFilterOperator ->
  -- | 'value'
  Prelude.Text ->
  InstanceIdFilter
newInstanceIdFilter pOperator_ pValue_ =
  InstanceIdFilter'
    { operator = pOperator_,
      value = pValue_
    }

-- | Undocumented member.
instanceIdFilter_operator :: Lens.Lens' InstanceIdFilter InstanceIdFilterOperator
instanceIdFilter_operator = Lens.lens (\InstanceIdFilter' {operator} -> operator) (\s@InstanceIdFilter' {} a -> s {operator = a} :: InstanceIdFilter)

-- | Undocumented member.
instanceIdFilter_value :: Lens.Lens' InstanceIdFilter Prelude.Text
instanceIdFilter_value = Lens.lens (\InstanceIdFilter' {value} -> value) (\s@InstanceIdFilter' {} a -> s {value = a} :: InstanceIdFilter)

instance Prelude.Hashable InstanceIdFilter where
  hashWithSalt _salt InstanceIdFilter' {..} =
    _salt `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData InstanceIdFilter where
  rnf InstanceIdFilter' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON InstanceIdFilter where
  toJSON InstanceIdFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("operator" Core..= operator),
            Prelude.Just ("value" Core..= value)
          ]
      )
