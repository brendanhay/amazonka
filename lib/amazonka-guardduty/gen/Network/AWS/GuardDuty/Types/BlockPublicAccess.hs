{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BlockPublicAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BlockPublicAccess
  ( BlockPublicAccess (..),

    -- * Smart constructor
    mkBlockPublicAccess,

    -- * Lenses
    bpaIgnorePublicACLs,
    bpaBlockPublicACLs,
    bpaRestrictPublicBuckets,
    bpaBlockPublicPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on how the bucker owner's S3 Block Public Access settings are being applied to the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html S3 Block Public Access> for more information.
--
-- /See:/ 'mkBlockPublicAccess' smart constructor.
data BlockPublicAccess = BlockPublicAccess'
  { ignorePublicACLs ::
      Lude.Maybe Lude.Bool,
    blockPublicACLs :: Lude.Maybe Lude.Bool,
    restrictPublicBuckets :: Lude.Maybe Lude.Bool,
    blockPublicPolicy :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockPublicAccess' with the minimum fields required to make a request.
--
-- * 'blockPublicACLs' - Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
-- * 'blockPublicPolicy' - Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
-- * 'ignorePublicACLs' - Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
-- * 'restrictPublicBuckets' - Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
mkBlockPublicAccess ::
  BlockPublicAccess
mkBlockPublicAccess =
  BlockPublicAccess'
    { ignorePublicACLs = Lude.Nothing,
      blockPublicACLs = Lude.Nothing,
      restrictPublicBuckets = Lude.Nothing,
      blockPublicPolicy = Lude.Nothing
    }

-- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
--
-- /Note:/ Consider using 'ignorePublicACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaIgnorePublicACLs :: Lens.Lens' BlockPublicAccess (Lude.Maybe Lude.Bool)
bpaIgnorePublicACLs = Lens.lens (ignorePublicACLs :: BlockPublicAccess -> Lude.Maybe Lude.Bool) (\s a -> s {ignorePublicACLs = a} :: BlockPublicAccess)
{-# DEPRECATED bpaIgnorePublicACLs "Use generic-lens or generic-optics with 'ignorePublicACLs' instead." #-}

-- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
--
-- /Note:/ Consider using 'blockPublicACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaBlockPublicACLs :: Lens.Lens' BlockPublicAccess (Lude.Maybe Lude.Bool)
bpaBlockPublicACLs = Lens.lens (blockPublicACLs :: BlockPublicAccess -> Lude.Maybe Lude.Bool) (\s a -> s {blockPublicACLs = a} :: BlockPublicAccess)
{-# DEPRECATED bpaBlockPublicACLs "Use generic-lens or generic-optics with 'blockPublicACLs' instead." #-}

-- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
--
-- /Note:/ Consider using 'restrictPublicBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaRestrictPublicBuckets :: Lens.Lens' BlockPublicAccess (Lude.Maybe Lude.Bool)
bpaRestrictPublicBuckets = Lens.lens (restrictPublicBuckets :: BlockPublicAccess -> Lude.Maybe Lude.Bool) (\s a -> s {restrictPublicBuckets = a} :: BlockPublicAccess)
{-# DEPRECATED bpaRestrictPublicBuckets "Use generic-lens or generic-optics with 'restrictPublicBuckets' instead." #-}

-- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaBlockPublicPolicy :: Lens.Lens' BlockPublicAccess (Lude.Maybe Lude.Bool)
bpaBlockPublicPolicy = Lens.lens (blockPublicPolicy :: BlockPublicAccess -> Lude.Maybe Lude.Bool) (\s a -> s {blockPublicPolicy = a} :: BlockPublicAccess)
{-# DEPRECATED bpaBlockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead." #-}

instance Lude.FromJSON BlockPublicAccess where
  parseJSON =
    Lude.withObject
      "BlockPublicAccess"
      ( \x ->
          BlockPublicAccess'
            Lude.<$> (x Lude..:? "ignorePublicAcls")
            Lude.<*> (x Lude..:? "blockPublicAcls")
            Lude.<*> (x Lude..:? "restrictPublicBuckets")
            Lude.<*> (x Lude..:? "blockPublicPolicy")
      )
