{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.PartialFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.PartialFailure
  ( PartialFailure (..),

    -- * Smart constructor
    mkPartialFailure,

    -- * Lenses
    pfExceptionType,
    pfFailureCode,
    pfFailureDescription,
    pfFailureResource,
  )
where

import qualified Network.AWS.CloudWatch.Types.ExceptionType as Types
import qualified Network.AWS.CloudWatch.Types.FailureCode as Types
import qualified Network.AWS.CloudWatch.Types.FailureDescription as Types
import qualified Network.AWS.CloudWatch.Types.FailureResource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This array is empty if the API operation was successful for all the rules specified in the request. If the operation could not process one of the rules, the following data is returned for each of those rules.
--
-- /See:/ 'mkPartialFailure' smart constructor.
data PartialFailure = PartialFailure'
  { -- | The type of error.
    exceptionType :: Core.Maybe Types.ExceptionType,
    -- | The code of the error.
    failureCode :: Core.Maybe Types.FailureCode,
    -- | A description of the error.
    failureDescription :: Core.Maybe Types.FailureDescription,
    -- | The specified rule that could not be deleted.
    failureResource :: Core.Maybe Types.FailureResource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartialFailure' value with any optional fields omitted.
mkPartialFailure ::
  PartialFailure
mkPartialFailure =
  PartialFailure'
    { exceptionType = Core.Nothing,
      failureCode = Core.Nothing,
      failureDescription = Core.Nothing,
      failureResource = Core.Nothing
    }

-- | The type of error.
--
-- /Note:/ Consider using 'exceptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfExceptionType :: Lens.Lens' PartialFailure (Core.Maybe Types.ExceptionType)
pfExceptionType = Lens.field @"exceptionType"
{-# DEPRECATED pfExceptionType "Use generic-lens or generic-optics with 'exceptionType' instead." #-}

-- | The code of the error.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureCode :: Lens.Lens' PartialFailure (Core.Maybe Types.FailureCode)
pfFailureCode = Lens.field @"failureCode"
{-# DEPRECATED pfFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | A description of the error.
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureDescription :: Lens.Lens' PartialFailure (Core.Maybe Types.FailureDescription)
pfFailureDescription = Lens.field @"failureDescription"
{-# DEPRECATED pfFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | The specified rule that could not be deleted.
--
-- /Note:/ Consider using 'failureResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureResource :: Lens.Lens' PartialFailure (Core.Maybe Types.FailureResource)
pfFailureResource = Lens.field @"failureResource"
{-# DEPRECATED pfFailureResource "Use generic-lens or generic-optics with 'failureResource' instead." #-}

instance Core.FromXML PartialFailure where
  parseXML x =
    PartialFailure'
      Core.<$> (x Core..@? "ExceptionType")
      Core.<*> (x Core..@? "FailureCode")
      Core.<*> (x Core..@? "FailureDescription")
      Core.<*> (x Core..@? "FailureResource")
