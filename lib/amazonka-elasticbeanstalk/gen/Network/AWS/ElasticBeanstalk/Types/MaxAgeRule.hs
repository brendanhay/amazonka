{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
  ( MaxAgeRule (..),

    -- * Smart constructor
    mkMaxAgeRule,

    -- * Lenses
    marDeleteSourceFromS3,
    marMaxAgeInDays,
    marEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A lifecycle rule that deletes application versions after the specified number of days.
--
-- /See:/ 'mkMaxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { deleteSourceFromS3 ::
      Lude.Maybe Lude.Bool,
    maxAgeInDays :: Lude.Maybe Lude.Int,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaxAgeRule' with the minimum fields required to make a request.
--
-- * 'deleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
-- * 'enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
-- * 'maxAgeInDays' - Specify the number of days to retain an application versions.
mkMaxAgeRule ::
  -- | 'enabled'
  Lude.Bool ->
  MaxAgeRule
mkMaxAgeRule pEnabled_ =
  MaxAgeRule'
    { deleteSourceFromS3 = Lude.Nothing,
      maxAgeInDays = Lude.Nothing,
      enabled = pEnabled_
    }

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- /Note:/ Consider using 'deleteSourceFromS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marDeleteSourceFromS3 :: Lens.Lens' MaxAgeRule (Lude.Maybe Lude.Bool)
marDeleteSourceFromS3 = Lens.lens (deleteSourceFromS3 :: MaxAgeRule -> Lude.Maybe Lude.Bool) (\s a -> s {deleteSourceFromS3 = a} :: MaxAgeRule)
{-# DEPRECATED marDeleteSourceFromS3 "Use generic-lens or generic-optics with 'deleteSourceFromS3' instead." #-}

-- | Specify the number of days to retain an application versions.
--
-- /Note:/ Consider using 'maxAgeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marMaxAgeInDays :: Lens.Lens' MaxAgeRule (Lude.Maybe Lude.Int)
marMaxAgeInDays = Lens.lens (maxAgeInDays :: MaxAgeRule -> Lude.Maybe Lude.Int) (\s a -> s {maxAgeInDays = a} :: MaxAgeRule)
{-# DEPRECATED marMaxAgeInDays "Use generic-lens or generic-optics with 'maxAgeInDays' instead." #-}

-- | Specify @true@ to apply the rule, or @false@ to disable it.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marEnabled :: Lens.Lens' MaxAgeRule Lude.Bool
marEnabled = Lens.lens (enabled :: MaxAgeRule -> Lude.Bool) (\s a -> s {enabled = a} :: MaxAgeRule)
{-# DEPRECATED marEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML MaxAgeRule where
  parseXML x =
    MaxAgeRule'
      Lude.<$> (x Lude..@? "DeleteSourceFromS3")
      Lude.<*> (x Lude..@? "MaxAgeInDays")
      Lude.<*> (x Lude..@ "Enabled")

instance Lude.ToQuery MaxAgeRule where
  toQuery MaxAgeRule' {..} =
    Lude.mconcat
      [ "DeleteSourceFromS3" Lude.=: deleteSourceFromS3,
        "MaxAgeInDays" Lude.=: maxAgeInDays,
        "Enabled" Lude.=: enabled
      ]
