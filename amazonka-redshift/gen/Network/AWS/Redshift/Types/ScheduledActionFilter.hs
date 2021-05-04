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
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduledActionFilterName

-- | A set of elements to filter the returned scheduled actions.
--
-- /See:/ 'newScheduledActionFilter' smart constructor.
data ScheduledActionFilter = ScheduledActionFilter'
  { -- | The type of element to filter.
    name :: ScheduledActionFilterName,
    -- | List of values. Compare if the value (of type defined by @Name@) equals
    -- an item in the list of scheduled actions.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledActionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'scheduledActionFilter_name' - The type of element to filter.
--
-- 'values', 'scheduledActionFilter_values' - List of values. Compare if the value (of type defined by @Name@) equals
-- an item in the list of scheduled actions.
newScheduledActionFilter ::
  -- | 'name'
  ScheduledActionFilterName ->
  ScheduledActionFilter
newScheduledActionFilter pName_ =
  ScheduledActionFilter'
    { name = pName_,
      values = Prelude.mempty
    }

-- | The type of element to filter.
scheduledActionFilter_name :: Lens.Lens' ScheduledActionFilter ScheduledActionFilterName
scheduledActionFilter_name = Lens.lens (\ScheduledActionFilter' {name} -> name) (\s@ScheduledActionFilter' {} a -> s {name = a} :: ScheduledActionFilter)

-- | List of values. Compare if the value (of type defined by @Name@) equals
-- an item in the list of scheduled actions.
scheduledActionFilter_values :: Lens.Lens' ScheduledActionFilter [Prelude.Text]
scheduledActionFilter_values = Lens.lens (\ScheduledActionFilter' {values} -> values) (\s@ScheduledActionFilter' {} a -> s {values = a} :: ScheduledActionFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable ScheduledActionFilter

instance Prelude.NFData ScheduledActionFilter

instance Prelude.ToQuery ScheduledActionFilter where
  toQuery ScheduledActionFilter' {..} =
    Prelude.mconcat
      [ "Name" Prelude.=: name,
        "Values"
          Prelude.=: Prelude.toQueryList "item" values
      ]
