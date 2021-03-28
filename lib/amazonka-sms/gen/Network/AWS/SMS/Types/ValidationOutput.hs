{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ValidationOutput
  ( ValidationOutput (..)
  -- * Smart constructor
  , mkValidationOutput
  -- * Lenses
  , voAppValidationOutput
  , voLatestValidationTime
  , voName
  , voServerValidationOutput
  , voStatus
  , voStatusMessage
  , voValidationId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.AppValidationOutput as Types
import qualified Network.AWS.SMS.Types.NonEmptyStringWithMaxLen255 as Types
import qualified Network.AWS.SMS.Types.ServerValidationOutput as Types
import qualified Network.AWS.SMS.Types.StatusMessage as Types
import qualified Network.AWS.SMS.Types.ValidationId as Types
import qualified Network.AWS.SMS.Types.ValidationStatus as Types

-- | Contains validation output.
--
-- /See:/ 'mkValidationOutput' smart constructor.
data ValidationOutput = ValidationOutput'
  { appValidationOutput :: Core.Maybe Types.AppValidationOutput
    -- ^ The output from validating an application.
  , latestValidationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The latest time that the validation was performed.
  , name :: Core.Maybe Types.NonEmptyStringWithMaxLen255
    -- ^ The name of the validation.
  , serverValidationOutput :: Core.Maybe Types.ServerValidationOutput
    -- ^ The output from validation an instance.
  , status :: Core.Maybe Types.ValidationStatus
    -- ^ The status of the validation.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ The status message.
  , validationId :: Core.Maybe Types.ValidationId
    -- ^ The ID of the validation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ValidationOutput' value with any optional fields omitted.
mkValidationOutput
    :: ValidationOutput
mkValidationOutput
  = ValidationOutput'{appValidationOutput = Core.Nothing,
                      latestValidationTime = Core.Nothing, name = Core.Nothing,
                      serverValidationOutput = Core.Nothing, status = Core.Nothing,
                      statusMessage = Core.Nothing, validationId = Core.Nothing}

-- | The output from validating an application.
--
-- /Note:/ Consider using 'appValidationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voAppValidationOutput :: Lens.Lens' ValidationOutput (Core.Maybe Types.AppValidationOutput)
voAppValidationOutput = Lens.field @"appValidationOutput"
{-# INLINEABLE voAppValidationOutput #-}
{-# DEPRECATED appValidationOutput "Use generic-lens or generic-optics with 'appValidationOutput' instead"  #-}

-- | The latest time that the validation was performed.
--
-- /Note:/ Consider using 'latestValidationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voLatestValidationTime :: Lens.Lens' ValidationOutput (Core.Maybe Core.NominalDiffTime)
voLatestValidationTime = Lens.field @"latestValidationTime"
{-# INLINEABLE voLatestValidationTime #-}
{-# DEPRECATED latestValidationTime "Use generic-lens or generic-optics with 'latestValidationTime' instead"  #-}

-- | The name of the validation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voName :: Lens.Lens' ValidationOutput (Core.Maybe Types.NonEmptyStringWithMaxLen255)
voName = Lens.field @"name"
{-# INLINEABLE voName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The output from validation an instance.
--
-- /Note:/ Consider using 'serverValidationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voServerValidationOutput :: Lens.Lens' ValidationOutput (Core.Maybe Types.ServerValidationOutput)
voServerValidationOutput = Lens.field @"serverValidationOutput"
{-# INLINEABLE voServerValidationOutput #-}
{-# DEPRECATED serverValidationOutput "Use generic-lens or generic-optics with 'serverValidationOutput' instead"  #-}

-- | The status of the validation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voStatus :: Lens.Lens' ValidationOutput (Core.Maybe Types.ValidationStatus)
voStatus = Lens.field @"status"
{-# INLINEABLE voStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voStatusMessage :: Lens.Lens' ValidationOutput (Core.Maybe Types.StatusMessage)
voStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE voStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voValidationId :: Lens.Lens' ValidationOutput (Core.Maybe Types.ValidationId)
voValidationId = Lens.field @"validationId"
{-# INLINEABLE voValidationId #-}
{-# DEPRECATED validationId "Use generic-lens or generic-optics with 'validationId' instead"  #-}

instance Core.FromJSON ValidationOutput where
        parseJSON
          = Core.withObject "ValidationOutput" Core.$
              \ x ->
                ValidationOutput' Core.<$>
                  (x Core..:? "appValidationOutput") Core.<*>
                    x Core..:? "latestValidationTime"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "serverValidationOutput"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusMessage"
                    Core.<*> x Core..:? "validationId"
