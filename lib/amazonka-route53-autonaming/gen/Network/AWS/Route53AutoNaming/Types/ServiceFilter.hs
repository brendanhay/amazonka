{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceFilter
  ( ServiceFilter (..),

    -- * Smart constructor
    mkServiceFilter,

    -- * Lenses
    sfValues,
    sfName,
    sfCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.ServiceFilterName

-- | A complex type that lets you specify the namespaces that you want to list services for.
--
-- /See:/ 'mkServiceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { -- | The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
    values :: [Lude.Text],
    -- | Specify @NAMESPACE_ID@ .
    name :: ServiceFilterName,
    -- | The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:
    --
    --
    --     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.
    --
    --
    --     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.
    --
    --
    --     * @BETWEEN@ : Not applicable.
    condition :: Lude.Maybe FilterCondition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceFilter' with the minimum fields required to make a request.
--
-- * 'values' - The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
-- * 'name' - Specify @NAMESPACE_ID@ .
-- * 'condition' - The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:
--
--
--     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.
--
--
--     * @BETWEEN@ : Not applicable.
mkServiceFilter ::
  -- | 'name'
  ServiceFilterName ->
  ServiceFilter
mkServiceFilter pName_ =
  ServiceFilter'
    { values = Lude.mempty,
      name = pName_,
      condition = Lude.Nothing
    }

-- | The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValues :: Lens.Lens' ServiceFilter [Lude.Text]
sfValues = Lens.lens (values :: ServiceFilter -> [Lude.Text]) (\s a -> s {values = a} :: ServiceFilter)
{-# DEPRECATED sfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Specify @NAMESPACE_ID@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' ServiceFilter ServiceFilterName
sfName = Lens.lens (name :: ServiceFilter -> ServiceFilterName) (\s a -> s {name = a} :: ServiceFilter)
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:
--
--
--     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.
--
--
--     * @BETWEEN@ : Not applicable.
--
--
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCondition :: Lens.Lens' ServiceFilter (Lude.Maybe FilterCondition)
sfCondition = Lens.lens (condition :: ServiceFilter -> Lude.Maybe FilterCondition) (\s a -> s {condition = a} :: ServiceFilter)
{-# DEPRECATED sfCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.ToJSON ServiceFilter where
  toJSON ServiceFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Name" Lude..= name),
            ("Condition" Lude..=) Lude.<$> condition
          ]
      )
