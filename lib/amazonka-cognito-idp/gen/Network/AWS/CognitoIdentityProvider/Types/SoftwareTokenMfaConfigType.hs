{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
  ( SoftwareTokenMfaConfigType (..)
  -- * Smart constructor
  , mkSoftwareTokenMfaConfigType
  -- * Lenses
  , stmctEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type used for enabling software token MFA at the user pool level.
--
-- /See:/ 'mkSoftwareTokenMfaConfigType' smart constructor.
newtype SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether software token MFA is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SoftwareTokenMfaConfigType' value with any optional fields omitted.
mkSoftwareTokenMfaConfigType
    :: SoftwareTokenMfaConfigType
mkSoftwareTokenMfaConfigType
  = SoftwareTokenMfaConfigType'{enabled = Core.Nothing}

-- | Specifies whether software token MFA is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmctEnabled :: Lens.Lens' SoftwareTokenMfaConfigType (Core.Maybe Core.Bool)
stmctEnabled = Lens.field @"enabled"
{-# INLINEABLE stmctEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON SoftwareTokenMfaConfigType where
        toJSON SoftwareTokenMfaConfigType{..}
          = Core.object
              (Core.catMaybes [("Enabled" Core..=) Core.<$> enabled])

instance Core.FromJSON SoftwareTokenMfaConfigType where
        parseJSON
          = Core.withObject "SoftwareTokenMfaConfigType" Core.$
              \ x -> SoftwareTokenMfaConfigType' Core.<$> (x Core..:? "Enabled")
