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
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.ServiceFilterName

-- | A complex type that lets you specify the namespaces that you want to
-- list services for.
--
-- /See:/ 'newServiceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { -- | The operator that you want to use to determine whether a service is
    -- returned by @ListServices@. Valid values for @Condition@ include the
    -- following:
    --
    -- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
    --     @EQ@ is the default condition and can be omitted.
    --
    -- -   @IN@: When you specify @IN@, specify a list of the IDs for the
    --     namespaces that you want @ListServices@ to return a list of services
    --     for.
    --
    -- -   @BETWEEN@: Not applicable.
    condition :: Core.Maybe FilterCondition,
    -- | Specify @NAMESPACE_ID@.
    name :: ServiceFilterName,
    -- | The values that are applicable to the value that you specify for
    -- @Condition@ to filter the list of services.
    values :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'serviceFilter_condition' - The operator that you want to use to determine whether a service is
-- returned by @ListServices@. Valid values for @Condition@ include the
-- following:
--
-- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @IN@: When you specify @IN@, specify a list of the IDs for the
--     namespaces that you want @ListServices@ to return a list of services
--     for.
--
-- -   @BETWEEN@: Not applicable.
--
-- 'name', 'serviceFilter_name' - Specify @NAMESPACE_ID@.
--
-- 'values', 'serviceFilter_values' - The values that are applicable to the value that you specify for
-- @Condition@ to filter the list of services.
newServiceFilter ::
  -- | 'name'
  ServiceFilterName ->
  ServiceFilter
newServiceFilter pName_ =
  ServiceFilter'
    { condition = Core.Nothing,
      name = pName_,
      values = Core.mempty
    }

-- | The operator that you want to use to determine whether a service is
-- returned by @ListServices@. Valid values for @Condition@ include the
-- following:
--
-- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @IN@: When you specify @IN@, specify a list of the IDs for the
--     namespaces that you want @ListServices@ to return a list of services
--     for.
--
-- -   @BETWEEN@: Not applicable.
serviceFilter_condition :: Lens.Lens' ServiceFilter (Core.Maybe FilterCondition)
serviceFilter_condition = Lens.lens (\ServiceFilter' {condition} -> condition) (\s@ServiceFilter' {} a -> s {condition = a} :: ServiceFilter)

-- | Specify @NAMESPACE_ID@.
serviceFilter_name :: Lens.Lens' ServiceFilter ServiceFilterName
serviceFilter_name = Lens.lens (\ServiceFilter' {name} -> name) (\s@ServiceFilter' {} a -> s {name = a} :: ServiceFilter)

-- | The values that are applicable to the value that you specify for
-- @Condition@ to filter the list of services.
serviceFilter_values :: Lens.Lens' ServiceFilter [Core.Text]
serviceFilter_values = Lens.lens (\ServiceFilter' {values} -> values) (\s@ServiceFilter' {} a -> s {values = a} :: ServiceFilter) Core.. Lens._Coerce

instance Core.Hashable ServiceFilter

instance Core.NFData ServiceFilter

instance Core.ToJSON ServiceFilter where
  toJSON ServiceFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Condition" Core..=) Core.<$> condition,
            Core.Just ("Name" Core..= name),
            Core.Just ("Values" Core..= values)
          ]
      )
