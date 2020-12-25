{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.LifecycleRule as Types

-- | Specifies the lifecycle configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkBucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { -- | A lifecycle rule for individual objects in an Amazon S3 bucket.
    rules :: [Types.LifecycleRule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'BucketLifecycleConfiguration' value with any optional fields omitted.
mkBucketLifecycleConfiguration ::
  BucketLifecycleConfiguration
mkBucketLifecycleConfiguration =
  BucketLifecycleConfiguration' {rules = Core.mempty}

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blcRules :: Lens.Lens' BucketLifecycleConfiguration [Types.LifecycleRule]
blcRules = Lens.field @"rules"
{-# DEPRECATED blcRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.ToXML BucketLifecycleConfiguration where
  toXML BucketLifecycleConfiguration {..} =
    Core.toXMLList "Rule" rules
