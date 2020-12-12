{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxCountRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxCountRule
  ( MaxCountRule (..),

    -- * Smart constructor
    mkMaxCountRule,

    -- * Lenses
    mcrMaxCount,
    mcrDeleteSourceFromS3,
    mcrEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.
--
-- /See:/ 'mkMaxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { maxCount :: Lude.Maybe Lude.Int,
    deleteSourceFromS3 :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'MaxCountRule' with the minimum fields required to make a request.
--
-- * 'deleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
-- * 'enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
-- * 'maxCount' - Specify the maximum number of application versions to retain.
mkMaxCountRule ::
  -- | 'enabled'
  Lude.Bool ->
  MaxCountRule
mkMaxCountRule pEnabled_ =
  MaxCountRule'
    { maxCount = Lude.Nothing,
      deleteSourceFromS3 = Lude.Nothing,
      enabled = pEnabled_
    }

-- | Specify the maximum number of application versions to retain.
--
-- /Note:/ Consider using 'maxCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrMaxCount :: Lens.Lens' MaxCountRule (Lude.Maybe Lude.Int)
mcrMaxCount = Lens.lens (maxCount :: MaxCountRule -> Lude.Maybe Lude.Int) (\s a -> s {maxCount = a} :: MaxCountRule)
{-# DEPRECATED mcrMaxCount "Use generic-lens or generic-optics with 'maxCount' instead." #-}

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- /Note:/ Consider using 'deleteSourceFromS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrDeleteSourceFromS3 :: Lens.Lens' MaxCountRule (Lude.Maybe Lude.Bool)
mcrDeleteSourceFromS3 = Lens.lens (deleteSourceFromS3 :: MaxCountRule -> Lude.Maybe Lude.Bool) (\s a -> s {deleteSourceFromS3 = a} :: MaxCountRule)
{-# DEPRECATED mcrDeleteSourceFromS3 "Use generic-lens or generic-optics with 'deleteSourceFromS3' instead." #-}

-- | Specify @true@ to apply the rule, or @false@ to disable it.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrEnabled :: Lens.Lens' MaxCountRule Lude.Bool
mcrEnabled = Lens.lens (enabled :: MaxCountRule -> Lude.Bool) (\s a -> s {enabled = a} :: MaxCountRule)
{-# DEPRECATED mcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML MaxCountRule where
  parseXML x =
    MaxCountRule'
      Lude.<$> (x Lude..@? "MaxCount")
      Lude.<*> (x Lude..@? "DeleteSourceFromS3")
      Lude.<*> (x Lude..@ "Enabled")

instance Lude.ToQuery MaxCountRule where
  toQuery MaxCountRule' {..} =
    Lude.mconcat
      [ "MaxCount" Lude.=: maxCount,
        "DeleteSourceFromS3" Lude.=: deleteSourceFromS3,
        "Enabled" Lude.=: enabled
      ]
