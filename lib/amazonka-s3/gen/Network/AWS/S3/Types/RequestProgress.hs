-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestProgress
  ( RequestProgress (..),

    -- * Smart constructor
    mkRequestProgress,

    -- * Lenses
    rpEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for specifying if periodic @QueryProgress@ messages should be sent.
--
-- /See:/ 'mkRequestProgress' smart constructor.
newtype RequestProgress = RequestProgress'
  { enabled ::
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

-- | Creates a value of 'RequestProgress' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
mkRequestProgress ::
  RequestProgress
mkRequestProgress = RequestProgress' {enabled = Lude.Nothing}

-- | Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpEnabled :: Lens.Lens' RequestProgress (Lude.Maybe Lude.Bool)
rpEnabled = Lens.lens (enabled :: RequestProgress -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: RequestProgress)
{-# DEPRECATED rpEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToXML RequestProgress where
  toXML RequestProgress' {..} =
    Lude.mconcat ["Enabled" Lude.@= enabled]
