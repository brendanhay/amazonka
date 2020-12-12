{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.BucketInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.BucketInfo
  ( BucketInfo (..),

    -- * Smart constructor
    mkBucketInfo,

    -- * Lenses
    biBuckets,
  )
where

import Network.AWS.CloudSearchDomains.Types.Bucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'mkBucketInfo' smart constructor.
newtype BucketInfo = BucketInfo' {buckets :: Lude.Maybe [Bucket]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BucketInfo' with the minimum fields required to make a request.
--
-- * 'buckets' - A list of the calculated facet values and counts.
mkBucketInfo ::
  BucketInfo
mkBucketInfo = BucketInfo' {buckets = Lude.Nothing}

-- | A list of the calculated facet values and counts.
--
-- /Note:/ Consider using 'buckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biBuckets :: Lens.Lens' BucketInfo (Lude.Maybe [Bucket])
biBuckets = Lens.lens (buckets :: BucketInfo -> Lude.Maybe [Bucket]) (\s a -> s {buckets = a} :: BucketInfo)
{-# DEPRECATED biBuckets "Use generic-lens or generic-optics with 'buckets' instead." #-}

instance Lude.FromJSON BucketInfo where
  parseJSON =
    Lude.withObject
      "BucketInfo"
      ( \x ->
          BucketInfo' Lude.<$> (x Lude..:? "buckets" Lude..!= Lude.mempty)
      )
