-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.PolicyStatus
  ( PolicyStatus (..),

    -- * Smart constructor
    mkPolicyStatus,

    -- * Lenses
    psIsPublic,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The container element for a bucket's policy status.
--
-- /See:/ 'mkPolicyStatus' smart constructor.
newtype PolicyStatus = PolicyStatus'
  { isPublic ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyStatus' with the minimum fields required to make a request.
--
-- * 'isPublic' - The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
mkPolicyStatus ::
  PolicyStatus
mkPolicyStatus = PolicyStatus' {isPublic = Lude.Nothing}

-- | The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
--
-- /Note:/ Consider using 'isPublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psIsPublic :: Lens.Lens' PolicyStatus (Lude.Maybe Lude.Bool)
psIsPublic = Lens.lens (isPublic :: PolicyStatus -> Lude.Maybe Lude.Bool) (\s a -> s {isPublic = a} :: PolicyStatus)
{-# DEPRECATED psIsPublic "Use generic-lens or generic-optics with 'isPublic' instead." #-}

instance Lude.FromXML PolicyStatus where
  parseXML x = PolicyStatus' Lude.<$> (x Lude..@? "IsPublic")
