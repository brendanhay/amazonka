{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RequestProgress
  ( RequestProgress (..)
  -- * Smart constructor
  , mkRequestProgress
  -- * Lenses
  , rpEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for specifying if periodic @QueryProgress@ messages should be sent.
--
-- /See:/ 'mkRequestProgress' smart constructor.
newtype RequestProgress = RequestProgress'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RequestProgress' value with any optional fields omitted.
mkRequestProgress
    :: RequestProgress
mkRequestProgress = RequestProgress'{enabled = Core.Nothing}

-- | Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpEnabled :: Lens.Lens' RequestProgress (Core.Maybe Core.Bool)
rpEnabled = Lens.field @"enabled"
{-# INLINEABLE rpEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToXML RequestProgress where
        toXML RequestProgress{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Enabled") enabled
