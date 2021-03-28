{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.FixedResponseActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.FixedResponseActionConfig
  ( FixedResponseActionConfig (..)
  -- * Smart constructor
  , mkFixedResponseActionConfig
  -- * Lenses
  , fracStatusCode
  , fracContentType
  , fracMessageBody
  ) where

import qualified Network.AWS.ELBv2.Types.FixedResponseActionContentType as Types
import qualified Network.AWS.ELBv2.Types.FixedResponseActionMessage as Types
import qualified Network.AWS.ELBv2.Types.FixedResponseActionStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an action that returns a custom HTTP response.
--
-- /See:/ 'mkFixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { statusCode :: Types.FixedResponseActionStatusCode
    -- ^ The HTTP response code (2XX, 4XX, or 5XX).
  , contentType :: Core.Maybe Types.FixedResponseActionContentType
    -- ^ The content type.
--
-- Valid Values: text/plain | text/css | text/html | application/javascript | application/json
  , messageBody :: Core.Maybe Types.FixedResponseActionMessage
    -- ^ The message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FixedResponseActionConfig' value with any optional fields omitted.
mkFixedResponseActionConfig
    :: Types.FixedResponseActionStatusCode -- ^ 'statusCode'
    -> FixedResponseActionConfig
mkFixedResponseActionConfig statusCode
  = FixedResponseActionConfig'{statusCode,
                               contentType = Core.Nothing, messageBody = Core.Nothing}

-- | The HTTP response code (2XX, 4XX, or 5XX).
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracStatusCode :: Lens.Lens' FixedResponseActionConfig Types.FixedResponseActionStatusCode
fracStatusCode = Lens.field @"statusCode"
{-# INLINEABLE fracStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The content type.
--
-- Valid Values: text/plain | text/css | text/html | application/javascript | application/json
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracContentType :: Lens.Lens' FixedResponseActionConfig (Core.Maybe Types.FixedResponseActionContentType)
fracContentType = Lens.field @"contentType"
{-# INLINEABLE fracContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The message.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracMessageBody :: Lens.Lens' FixedResponseActionConfig (Core.Maybe Types.FixedResponseActionMessage)
fracMessageBody = Lens.field @"messageBody"
{-# INLINEABLE fracMessageBody #-}
{-# DEPRECATED messageBody "Use generic-lens or generic-optics with 'messageBody' instead"  #-}

instance Core.ToQuery FixedResponseActionConfig where
        toQuery FixedResponseActionConfig{..}
          = Core.toQueryPair "StatusCode" statusCode Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ContentType") contentType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MessageBody") messageBody

instance Core.FromXML FixedResponseActionConfig where
        parseXML x
          = FixedResponseActionConfig' Core.<$>
              (x Core..@ "StatusCode") Core.<*> x Core..@? "ContentType" Core.<*>
                x Core..@? "MessageBody"
