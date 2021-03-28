{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ContinuationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ContinuationEvent
  ( ContinuationEvent (..)
  -- * Smart constructor
  , mkContinuationEvent
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | 
--
-- /See:/ 'mkContinuationEvent' smart constructor.
data ContinuationEvent = ContinuationEvent'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinuationEvent' value with any optional fields omitted.
mkContinuationEvent
    :: ContinuationEvent
mkContinuationEvent = ContinuationEvent'

instance Core.FromXML ContinuationEvent where
        parseXML x = Core.pure ContinuationEvent'
