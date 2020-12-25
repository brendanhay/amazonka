{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CreateBucketConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CreateBucketConfiguration
  ( CreateBucketConfiguration (..),

    -- * Smart constructor
    mkCreateBucketConfiguration,

    -- * Lenses
    cbcLocationConstraint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The configuration information for the bucket.
--
-- /See:/ 'mkCreateBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
  { -- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
    locationConstraint :: Core.Maybe Types.LocationConstraint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBucketConfiguration' value with any optional fields omitted.
mkCreateBucketConfiguration ::
  CreateBucketConfiguration
mkCreateBucketConfiguration =
  CreateBucketConfiguration' {locationConstraint = Core.Nothing}

-- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
--
-- /Note:/ Consider using 'locationConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcLocationConstraint :: Lens.Lens' CreateBucketConfiguration (Core.Maybe Types.LocationConstraint)
cbcLocationConstraint = Lens.field @"locationConstraint"
{-# DEPRECATED cbcLocationConstraint "Use generic-lens or generic-optics with 'locationConstraint' instead." #-}

instance Core.ToXML CreateBucketConfiguration where
  toXML CreateBucketConfiguration {..} =
    Core.toXMLNode "LocationConstraint" Core.<$> locationConstraint
