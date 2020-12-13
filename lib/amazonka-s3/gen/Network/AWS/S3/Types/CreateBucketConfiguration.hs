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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The configuration information for the bucket.
--
-- /See:/ 'mkCreateBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
  { -- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
    locationConstraint :: Lude.Maybe LocationConstraint
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBucketConfiguration' with the minimum fields required to make a request.
--
-- * 'locationConstraint' - Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
mkCreateBucketConfiguration ::
  CreateBucketConfiguration
mkCreateBucketConfiguration =
  CreateBucketConfiguration' {locationConstraint = Lude.Nothing}

-- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
--
-- /Note:/ Consider using 'locationConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcLocationConstraint :: Lens.Lens' CreateBucketConfiguration (Lude.Maybe LocationConstraint)
cbcLocationConstraint = Lens.lens (locationConstraint :: CreateBucketConfiguration -> Lude.Maybe LocationConstraint) (\s a -> s {locationConstraint = a} :: CreateBucketConfiguration)
{-# DEPRECATED cbcLocationConstraint "Use generic-lens or generic-optics with 'locationConstraint' instead." #-}

instance Lude.ToXML CreateBucketConfiguration where
  toXML CreateBucketConfiguration' {..} =
    Lude.mconcat ["LocationConstraint" Lude.@= locationConstraint]
