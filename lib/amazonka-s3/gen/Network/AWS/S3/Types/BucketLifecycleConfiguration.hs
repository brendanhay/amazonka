-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLifecycleConfiguration
  ( BucketLifecycleConfiguration (..),

    -- * Smart constructor
    mkBucketLifecycleConfiguration,

    -- * Lenses
    blcRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRule

-- | Specifies the lifecycle configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkBucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { rules ::
      [LifecycleRule]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- * 'rules' - A lifecycle rule for individual objects in an Amazon S3 bucket.
mkBucketLifecycleConfiguration ::
  BucketLifecycleConfiguration
mkBucketLifecycleConfiguration =
  BucketLifecycleConfiguration' {rules = Lude.mempty}

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blcRules :: Lens.Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = Lens.lens (rules :: BucketLifecycleConfiguration -> [LifecycleRule]) (\s a -> s {rules = a} :: BucketLifecycleConfiguration)
{-# DEPRECATED blcRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.ToXML BucketLifecycleConfiguration where
  toXML BucketLifecycleConfiguration' {..} =
    Lude.mconcat [Lude.toXMLList "Rule" rules]
