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
-- Module      : Network.AWS.AutoScaling.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Filter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a filter that is used to return a more specific list of
-- results when describing tags.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | One or more filter values. Filter values are case-sensitive.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter. The valid values are: @auto-scaling-group@,
    -- @key@, @value@, and @propagate-at-launch@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'filter_values' - One or more filter values. Filter values are case-sensitive.
--
-- 'name', 'filter_name' - The name of the filter. The valid values are: @auto-scaling-group@,
-- @key@, @value@, and @propagate-at-launch@.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter' {values = Prelude.Nothing, name = pName_}

-- | One or more filter values. Filter values are case-sensitive.
filter_values :: Lens.Lens' Filter (Prelude.Maybe [Prelude.Text])
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the filter. The valid values are: @auto-scaling-group@,
-- @key@, @value@, and @propagate-at-launch@.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Prelude.Hashable Filter

instance Prelude.NFData Filter

instance Prelude.ToQuery Filter where
  toQuery Filter' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values),
        "Name" Prelude.=: name
      ]
