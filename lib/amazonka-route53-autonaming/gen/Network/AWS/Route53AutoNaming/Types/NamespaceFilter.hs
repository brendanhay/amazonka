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
    nfValues,
    nfName,
    nfCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.NamespaceFilterName

-- | A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.
--
-- /See:/ 'mkNamespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { -- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ .
    --
    -- If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
    values :: [Lude.Text],
    -- | Specify @TYPE@ .
    name :: NamespaceFilterName,
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
    condition :: Lude.Maybe FilterCondition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NamespaceFilter' with the minimum fields required to make a request.
--
-- * 'values' - If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ .
--
-- If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
-- * 'name' - Specify @TYPE@ .
-- * 'condition' - The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:
--
--
--     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.
--
--
--     * @BETWEEN@ : Not applicable
mkNamespaceFilter ::
  -- | 'name'
  NamespaceFilterName ->
  NamespaceFilter
mkNamespaceFilter pName_ =
  NamespaceFilter'
    { values = Lude.mempty,
      name = pName_,
      condition = Lude.Nothing
    }

-- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ .
--
-- If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfValues :: Lens.Lens' NamespaceFilter [Lude.Text]
nfValues = Lens.lens (values :: NamespaceFilter -> [Lude.Text]) (\s a -> s {values = a} :: NamespaceFilter)
{-# DEPRECATED nfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Specify @TYPE@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfName :: Lens.Lens' NamespaceFilter NamespaceFilterName
nfName = Lens.lens (name :: NamespaceFilter -> NamespaceFilterName) (\s a -> s {name = a} :: NamespaceFilter)
{-# DEPRECATED nfName "Use generic-lens or generic-optics with 'name' instead." #-}

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
nfCondition :: Lens.Lens' NamespaceFilter (Lude.Maybe FilterCondition)
nfCondition = Lens.lens (condition :: NamespaceFilter -> Lude.Maybe FilterCondition) (\s a -> s {condition = a} :: NamespaceFilter)
{-# DEPRECATED nfCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.ToJSON NamespaceFilter where
  toJSON NamespaceFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Name" Lude..= name),
            ("Condition" Lude..=) Lude.<$> condition
          ]
      )
