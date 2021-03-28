{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
  ( BatchPutMessageErrorEntry (..)
  -- * Smart constructor
  , mkBatchPutMessageErrorEntry
  -- * Lenses
  , bpmeeErrorCode
  , bpmeeErrorMessage
  , bpmeeMessageId
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ErrorCode as Types
import qualified Network.AWS.IoTAnalytics.Types.ErrorMessage as Types
import qualified Network.AWS.IoTAnalytics.Types.MessageId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains informations about errors.
--
-- /See:/ 'mkBatchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The code associated with the error.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The message associated with the error.
  , messageId :: Core.Maybe Types.MessageId
    -- ^ The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutMessageErrorEntry' value with any optional fields omitted.
mkBatchPutMessageErrorEntry
    :: BatchPutMessageErrorEntry
mkBatchPutMessageErrorEntry
  = BatchPutMessageErrorEntry'{errorCode = Core.Nothing,
                               errorMessage = Core.Nothing, messageId = Core.Nothing}

-- | The code associated with the error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeErrorCode :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Types.ErrorCode)
bpmeeErrorCode = Lens.field @"errorCode"
{-# INLINEABLE bpmeeErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The message associated with the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeErrorMessage :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Types.ErrorMessage)
bpmeeErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE bpmeeErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeMessageId :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Types.MessageId)
bpmeeMessageId = Lens.field @"messageId"
{-# INLINEABLE bpmeeMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

instance Core.FromJSON BatchPutMessageErrorEntry where
        parseJSON
          = Core.withObject "BatchPutMessageErrorEntry" Core.$
              \ x ->
                BatchPutMessageErrorEntry' Core.<$>
                  (x Core..:? "errorCode") Core.<*> x Core..:? "errorMessage"
                    Core.<*> x Core..:? "messageId"
