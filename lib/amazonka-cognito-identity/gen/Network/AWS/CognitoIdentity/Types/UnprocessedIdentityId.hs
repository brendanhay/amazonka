{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
  ( UnprocessedIdentityId (..),

    -- * Smart constructor
    mkUnprocessedIdentityId,

    -- * Lenses
    uiiErrorCode,
    uiiIdentityId,
  )
where

import qualified Network.AWS.CognitoIdentity.Types.CognitoErrorCode as Types
import qualified Network.AWS.CognitoIdentity.Types.IdentityId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
-- /See:/ 'mkUnprocessedIdentityId' smart constructor.
data UnprocessedIdentityId = UnprocessedIdentityId'
  { -- | The error code indicating the type of error that occurred.
    errorCode :: Core.Maybe Types.CognitoErrorCode,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Types.IdentityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedIdentityId' value with any optional fields omitted.
mkUnprocessedIdentityId ::
  UnprocessedIdentityId
mkUnprocessedIdentityId =
  UnprocessedIdentityId'
    { errorCode = Core.Nothing,
      identityId = Core.Nothing
    }

-- | The error code indicating the type of error that occurred.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiErrorCode :: Lens.Lens' UnprocessedIdentityId (Core.Maybe Types.CognitoErrorCode)
uiiErrorCode = Lens.field @"errorCode"
{-# DEPRECATED uiiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiIdentityId :: Lens.Lens' UnprocessedIdentityId (Core.Maybe Types.IdentityId)
uiiIdentityId = Lens.field @"identityId"
{-# DEPRECATED uiiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Core.FromJSON UnprocessedIdentityId where
  parseJSON =
    Core.withObject "UnprocessedIdentityId" Core.$
      \x ->
        UnprocessedIdentityId'
          Core.<$> (x Core..:? "ErrorCode") Core.<*> (x Core..:? "IdentityId")
