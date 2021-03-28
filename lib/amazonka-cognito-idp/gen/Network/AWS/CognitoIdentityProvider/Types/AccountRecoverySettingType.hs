{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
  ( AccountRecoverySettingType (..)
  -- * Smart constructor
  , mkAccountRecoverySettingType
  -- * Lenses
  , arstRecoveryMechanisms
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The data type for @AccountRecoverySetting@ .
--
-- /See:/ 'mkAccountRecoverySettingType' smart constructor.
newtype AccountRecoverySettingType = AccountRecoverySettingType'
  { recoveryMechanisms :: Core.Maybe (Core.NonEmpty Types.RecoveryOptionType)
    -- ^ The list of @RecoveryOptionTypes@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccountRecoverySettingType' value with any optional fields omitted.
mkAccountRecoverySettingType
    :: AccountRecoverySettingType
mkAccountRecoverySettingType
  = AccountRecoverySettingType'{recoveryMechanisms = Core.Nothing}

-- | The list of @RecoveryOptionTypes@ .
--
-- /Note:/ Consider using 'recoveryMechanisms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arstRecoveryMechanisms :: Lens.Lens' AccountRecoverySettingType (Core.Maybe (Core.NonEmpty Types.RecoveryOptionType))
arstRecoveryMechanisms = Lens.field @"recoveryMechanisms"
{-# INLINEABLE arstRecoveryMechanisms #-}
{-# DEPRECATED recoveryMechanisms "Use generic-lens or generic-optics with 'recoveryMechanisms' instead"  #-}

instance Core.FromJSON AccountRecoverySettingType where
        toJSON AccountRecoverySettingType{..}
          = Core.object
              (Core.catMaybes
                 [("RecoveryMechanisms" Core..=) Core.<$> recoveryMechanisms])

instance Core.FromJSON AccountRecoverySettingType where
        parseJSON
          = Core.withObject "AccountRecoverySettingType" Core.$
              \ x ->
                AccountRecoverySettingType' Core.<$>
                  (x Core..:? "RecoveryMechanisms")
