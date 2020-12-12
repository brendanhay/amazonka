{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLoggingStatus
  ( BucketLoggingStatus (..),

    -- * Smart constructor
    mkBucketLoggingStatus,

    -- * Lenses
    blsLoggingEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LoggingEnabled

-- | Container for logging status information.
--
-- /See:/ 'mkBucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
  { loggingEnabled ::
      Lude.Maybe LoggingEnabled
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BucketLoggingStatus' with the minimum fields required to make a request.
--
-- * 'loggingEnabled' - Undocumented field.
mkBucketLoggingStatus ::
  BucketLoggingStatus
mkBucketLoggingStatus =
  BucketLoggingStatus' {loggingEnabled = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blsLoggingEnabled :: Lens.Lens' BucketLoggingStatus (Lude.Maybe LoggingEnabled)
blsLoggingEnabled = Lens.lens (loggingEnabled :: BucketLoggingStatus -> Lude.Maybe LoggingEnabled) (\s a -> s {loggingEnabled = a} :: BucketLoggingStatus)
{-# DEPRECATED blsLoggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead." #-}

instance Lude.ToXML BucketLoggingStatus where
  toXML BucketLoggingStatus' {..} =
    Lude.mconcat ["LoggingEnabled" Lude.@= loggingEnabled]
