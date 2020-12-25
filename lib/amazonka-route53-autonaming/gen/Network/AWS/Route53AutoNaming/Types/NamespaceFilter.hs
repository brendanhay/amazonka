{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceFilter
  ( NamespaceFilter (..),

    -- * Smart constructor
    mkNamespaceFilter,

    -- * Lenses
    nfName,
    nfValues,
    nfCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.FilterCondition as Types
import qualified Network.AWS.Route53AutoNaming.Types.FilterValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceFilterName as Types

-- | A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.
--
-- /See:/ 'mkNamespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { -- | Specify @TYPE@ .
    name :: Types.NamespaceFilterName,
    -- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ .
    --
    -- If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
    values :: [Types.FilterValue],
    -- | The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:
    --
    --
    --     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.
    --
    --
    --     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.
    --
    --
    --     * @BETWEEN@ : Not applicable
    condition :: Core.Maybe Types.FilterCondition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NamespaceFilter' value with any optional fields omitted.
mkNamespaceFilter ::
  -- | 'name'
  Types.NamespaceFilterName ->
  NamespaceFilter
mkNamespaceFilter name =
  NamespaceFilter'
    { name,
      values = Core.mempty,
      condition = Core.Nothing
    }

-- | Specify @TYPE@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfName :: Lens.Lens' NamespaceFilter Types.NamespaceFilterName
nfName = Lens.field @"name"
{-# DEPRECATED nfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ .
--
-- If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfValues :: Lens.Lens' NamespaceFilter [Types.FilterValue]
nfValues = Lens.field @"values"
{-# DEPRECATED nfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:
--
--
--     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.
--
--
--     * @BETWEEN@ : Not applicable
--
--
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfCondition :: Lens.Lens' NamespaceFilter (Core.Maybe Types.FilterCondition)
nfCondition = Lens.field @"condition"
{-# DEPRECATED nfCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Core.FromJSON NamespaceFilter where
  toJSON NamespaceFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Values" Core..= values),
            ("Condition" Core..=) Core.<$> condition
          ]
      )
