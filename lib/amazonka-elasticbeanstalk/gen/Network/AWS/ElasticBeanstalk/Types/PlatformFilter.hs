-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFilter
  ( PlatformFilter (..),

    -- * Smart constructor
    mkPlatformFilter,

    -- * Lenses
    pfValues,
    pfOperator,
    pfType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes criteria to restrict the results when listing platform versions.
--
-- The filter is evaluated as follows: @Type Operator Values[1]@
--
-- /See:/ 'mkPlatformFilter' smart constructor.
data PlatformFilter = PlatformFilter'
  { values ::
      Lude.Maybe [Lude.Text],
    operator :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformFilter' with the minimum fields required to make a request.
--
-- * 'operator' - The operator to apply to the @Type@ with each of the @Values@ .
--
-- Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@
-- * 'type'' - The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@
-- * 'values' - The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
--     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@
--
--
--     * @PlatformLifecycleState@ : @recommended@
--
--
--     * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@
--
--
--     * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@
mkPlatformFilter ::
  PlatformFilter
mkPlatformFilter =
  PlatformFilter'
    { values = Lude.Nothing,
      operator = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
--     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@
--
--
--     * @PlatformLifecycleState@ : @recommended@
--
--
--     * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@
--
--
--     * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfValues :: Lens.Lens' PlatformFilter (Lude.Maybe [Lude.Text])
pfValues = Lens.lens (values :: PlatformFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: PlatformFilter)
{-# DEPRECATED pfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The operator to apply to the @Type@ with each of the @Values@ .
--
-- Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfOperator :: Lens.Lens' PlatformFilter (Lude.Maybe Lude.Text)
pfOperator = Lens.lens (operator :: PlatformFilter -> Lude.Maybe Lude.Text) (\s a -> s {operator = a} :: PlatformFilter)
{-# DEPRECATED pfOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfType :: Lens.Lens' PlatformFilter (Lude.Maybe Lude.Text)
pfType = Lens.lens (type' :: PlatformFilter -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: PlatformFilter)
{-# DEPRECATED pfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToQuery PlatformFilter where
  toQuery PlatformFilter' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "Operator" Lude.=: operator,
        "Type" Lude.=: type'
      ]
