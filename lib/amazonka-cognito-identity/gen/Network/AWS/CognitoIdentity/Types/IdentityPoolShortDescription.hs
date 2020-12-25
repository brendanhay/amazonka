{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
  ( IdentityPoolShortDescription (..),

    -- * Smart constructor
    mkIdentityPoolShortDescription,

    -- * Lenses
    ipsdIdentityPoolId,
    ipsdIdentityPoolName,
  )
where

import qualified Network.AWS.CognitoIdentity.Types.IdentityPoolId as Types
import qualified Network.AWS.CognitoIdentity.Types.IdentityPoolName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A description of the identity pool.
--
-- /See:/ 'mkIdentityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | A string that you provide.
    identityPoolName :: Core.Maybe Types.IdentityPoolName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IdentityPoolShortDescription' value with any optional fields omitted.
mkIdentityPoolShortDescription ::
  IdentityPoolShortDescription
mkIdentityPoolShortDescription =
  IdentityPoolShortDescription'
    { identityPoolId = Core.Nothing,
      identityPoolName = Core.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdIdentityPoolId :: Lens.Lens' IdentityPoolShortDescription (Core.Maybe Types.IdentityPoolId)
ipsdIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED ipsdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdIdentityPoolName :: Lens.Lens' IdentityPoolShortDescription (Core.Maybe Types.IdentityPoolName)
ipsdIdentityPoolName = Lens.field @"identityPoolName"
{-# DEPRECATED ipsdIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

instance Core.FromJSON IdentityPoolShortDescription where
  parseJSON =
    Core.withObject "IdentityPoolShortDescription" Core.$
      \x ->
        IdentityPoolShortDescription'
          Core.<$> (x Core..:? "IdentityPoolId")
          Core.<*> (x Core..:? "IdentityPoolName")
