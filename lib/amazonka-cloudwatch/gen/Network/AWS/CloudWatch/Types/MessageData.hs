{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.MessageData
  ( MessageData (..)
  -- * Smart constructor
  , mkMessageData
  -- * Lenses
  , mCode
  , mValue
  ) where

import qualified Network.AWS.CloudWatch.Types.Code as Types
import qualified Network.AWS.CloudWatch.Types.MessageDataValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A message returned by the @GetMetricData@ API, including a code and a description.
--
-- /See:/ 'mkMessageData' smart constructor.
data MessageData = MessageData'
  { code :: Core.Maybe Types.Code
    -- ^ The error code or status code associated with the message.
  , value :: Core.Maybe Types.MessageDataValue
    -- ^ The message text.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageData' value with any optional fields omitted.
mkMessageData
    :: MessageData
mkMessageData
  = MessageData'{code = Core.Nothing, value = Core.Nothing}

-- | The error code or status code associated with the message.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCode :: Lens.Lens' MessageData (Core.Maybe Types.Code)
mCode = Lens.field @"code"
{-# INLINEABLE mCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The message text.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' MessageData (Core.Maybe Types.MessageDataValue)
mValue = Lens.field @"value"
{-# INLINEABLE mValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML MessageData where
        parseXML x
          = MessageData' Core.<$>
              (x Core..@? "Code") Core.<*> x Core..@? "Value"
