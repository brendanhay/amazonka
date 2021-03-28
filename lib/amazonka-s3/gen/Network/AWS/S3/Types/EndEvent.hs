{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EndEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.EndEvent
  ( EndEvent (..)
  -- * Smart constructor
  , mkEndEvent
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | A message that indicates the request is complete and no more messages will be sent. You should not assume that the request is complete until the client receives an @EndEvent@ .
--
-- /See:/ 'mkEndEvent' smart constructor.
data EndEvent = EndEvent'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndEvent' value with any optional fields omitted.
mkEndEvent
    :: EndEvent
mkEndEvent = EndEvent'

instance Core.FromXML EndEvent where
        parseXML x = Core.pure EndEvent'
