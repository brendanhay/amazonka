{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AccountModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.AccountModification
  ( AccountModification (..)
  -- * Smart constructor
  , mkAccountModification
  -- * Lenses
  , amDedicatedTenancyManagementCidrRange
  , amDedicatedTenancySupport
  , amErrorCode
  , amErrorMessage
  , amModificationState
  , amStartTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.DedicatedTenancyManagementCidrRange as Types
import qualified Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum as Types
import qualified Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorCode as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorMessage as Types

-- | Describes a modification to the configuration of Bring Your Own License (BYOL) for the specified account. 
--
-- /See:/ 'mkAccountModification' smart constructor.
data AccountModification = AccountModification'
  { dedicatedTenancyManagementCidrRange :: Core.Maybe Types.DedicatedTenancyManagementCidrRange
    -- ^ The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
  , dedicatedTenancySupport :: Core.Maybe Types.DedicatedTenancySupportResultEnum
    -- ^ The status of BYOL (whether BYOL is being enabled or disabled).
  , errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that is returned if the configuration of BYOL cannot be modified.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The text of the error message that is returned if the configuration of BYOL cannot be modified.
  , modificationState :: Core.Maybe Types.DedicatedTenancyModificationStateEnum
    -- ^ The state of the modification to the configuration of BYOL.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the modification of the BYOL configuration was started.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AccountModification' value with any optional fields omitted.
mkAccountModification
    :: AccountModification
mkAccountModification
  = AccountModification'{dedicatedTenancyManagementCidrRange =
                           Core.Nothing,
                         dedicatedTenancySupport = Core.Nothing, errorCode = Core.Nothing,
                         errorMessage = Core.Nothing, modificationState = Core.Nothing,
                         startTime = Core.Nothing}

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDedicatedTenancyManagementCidrRange :: Lens.Lens' AccountModification (Core.Maybe Types.DedicatedTenancyManagementCidrRange)
amDedicatedTenancyManagementCidrRange = Lens.field @"dedicatedTenancyManagementCidrRange"
{-# INLINEABLE amDedicatedTenancyManagementCidrRange #-}
{-# DEPRECATED dedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead"  #-}

-- | The status of BYOL (whether BYOL is being enabled or disabled).
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDedicatedTenancySupport :: Lens.Lens' AccountModification (Core.Maybe Types.DedicatedTenancySupportResultEnum)
amDedicatedTenancySupport = Lens.field @"dedicatedTenancySupport"
{-# INLINEABLE amDedicatedTenancySupport #-}
{-# DEPRECATED dedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead"  #-}

-- | The error code that is returned if the configuration of BYOL cannot be modified.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amErrorCode :: Lens.Lens' AccountModification (Core.Maybe Types.ErrorCode)
amErrorCode = Lens.field @"errorCode"
{-# INLINEABLE amErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The text of the error message that is returned if the configuration of BYOL cannot be modified.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amErrorMessage :: Lens.Lens' AccountModification (Core.Maybe Types.ErrorMessage)
amErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE amErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The state of the modification to the configuration of BYOL.
--
-- /Note:/ Consider using 'modificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amModificationState :: Lens.Lens' AccountModification (Core.Maybe Types.DedicatedTenancyModificationStateEnum)
amModificationState = Lens.field @"modificationState"
{-# INLINEABLE amModificationState #-}
{-# DEPRECATED modificationState "Use generic-lens or generic-optics with 'modificationState' instead"  #-}

-- | The timestamp when the modification of the BYOL configuration was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amStartTime :: Lens.Lens' AccountModification (Core.Maybe Core.NominalDiffTime)
amStartTime = Lens.field @"startTime"
{-# INLINEABLE amStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.FromJSON AccountModification where
        parseJSON
          = Core.withObject "AccountModification" Core.$
              \ x ->
                AccountModification' Core.<$>
                  (x Core..:? "DedicatedTenancyManagementCidrRange") Core.<*>
                    x Core..:? "DedicatedTenancySupport"
                    Core.<*> x Core..:? "ErrorCode"
                    Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "ModificationState"
                    Core.<*> x Core..:? "StartTime"
