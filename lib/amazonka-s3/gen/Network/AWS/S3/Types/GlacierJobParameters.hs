{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.GlacierJobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.GlacierJobParameters
  ( GlacierJobParameters (..),

    -- * Smart constructor
    mkGlacierJobParameters,

    -- * Lenses
    gjpTier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tier

-- | Container for S3 Glacier job parameters.
--
-- /See:/ 'mkGlacierJobParameters' smart constructor.
newtype GlacierJobParameters = GlacierJobParameters' {tier :: Tier}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlacierJobParameters' with the minimum fields required to make a request.
--
-- * 'tier' - Retrieval tier at which the restore will be processed.
mkGlacierJobParameters ::
  -- | 'tier'
  Tier ->
  GlacierJobParameters
mkGlacierJobParameters pTier_ =
  GlacierJobParameters' {tier = pTier_}

-- | Retrieval tier at which the restore will be processed.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjpTier :: Lens.Lens' GlacierJobParameters Tier
gjpTier = Lens.lens (tier :: GlacierJobParameters -> Tier) (\s a -> s {tier = a} :: GlacierJobParameters)
{-# DEPRECATED gjpTier "Use generic-lens or generic-optics with 'tier' instead." #-}

instance Lude.ToXML GlacierJobParameters where
  toXML GlacierJobParameters' {..} =
    Lude.mconcat ["Tier" Lude.@= tier]
