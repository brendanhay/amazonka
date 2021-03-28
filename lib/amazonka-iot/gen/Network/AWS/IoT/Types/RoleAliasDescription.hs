{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RoleAliasDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.RoleAliasDescription
  ( RoleAliasDescription (..)
  -- * Smart constructor
  , mkRoleAliasDescription
  -- * Lenses
  , radCreationDate
  , radCredentialDurationSeconds
  , radLastModifiedDate
  , radOwner
  , radRoleAlias
  , radRoleAliasArn
  , radRoleArn
  ) where

import qualified Network.AWS.IoT.Types.Owner as Types
import qualified Network.AWS.IoT.Types.RoleAlias as Types
import qualified Network.AWS.IoT.Types.RoleAliasArn as Types
import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Role alias description.
--
-- /See:/ 'mkRoleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The UNIX timestamp of when the role alias was created.
  , credentialDurationSeconds :: Core.Maybe Core.Natural
    -- ^ The number of seconds for which the credential is valid.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The UNIX timestamp of when the role alias was last modified.
  , owner :: Core.Maybe Types.Owner
    -- ^ The role alias owner.
  , roleAlias :: Core.Maybe Types.RoleAlias
    -- ^ The role alias.
  , roleAliasArn :: Core.Maybe Types.RoleAliasArn
    -- ^ The ARN of the role alias.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The role ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RoleAliasDescription' value with any optional fields omitted.
mkRoleAliasDescription
    :: RoleAliasDescription
mkRoleAliasDescription
  = RoleAliasDescription'{creationDate = Core.Nothing,
                          credentialDurationSeconds = Core.Nothing,
                          lastModifiedDate = Core.Nothing, owner = Core.Nothing,
                          roleAlias = Core.Nothing, roleAliasArn = Core.Nothing,
                          roleArn = Core.Nothing}

-- | The UNIX timestamp of when the role alias was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radCreationDate :: Lens.Lens' RoleAliasDescription (Core.Maybe Core.NominalDiffTime)
radCreationDate = Lens.field @"creationDate"
{-# INLINEABLE radCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The number of seconds for which the credential is valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radCredentialDurationSeconds :: Lens.Lens' RoleAliasDescription (Core.Maybe Core.Natural)
radCredentialDurationSeconds = Lens.field @"credentialDurationSeconds"
{-# INLINEABLE radCredentialDurationSeconds #-}
{-# DEPRECATED credentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead"  #-}

-- | The UNIX timestamp of when the role alias was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radLastModifiedDate :: Lens.Lens' RoleAliasDescription (Core.Maybe Core.NominalDiffTime)
radLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE radLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The role alias owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radOwner :: Lens.Lens' RoleAliasDescription (Core.Maybe Types.Owner)
radOwner = Lens.field @"owner"
{-# INLINEABLE radOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleAlias :: Lens.Lens' RoleAliasDescription (Core.Maybe Types.RoleAlias)
radRoleAlias = Lens.field @"roleAlias"
{-# INLINEABLE radRoleAlias #-}
{-# DEPRECATED roleAlias "Use generic-lens or generic-optics with 'roleAlias' instead"  #-}

-- | The ARN of the role alias.
--
-- /Note:/ Consider using 'roleAliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleAliasArn :: Lens.Lens' RoleAliasDescription (Core.Maybe Types.RoleAliasArn)
radRoleAliasArn = Lens.field @"roleAliasArn"
{-# INLINEABLE radRoleAliasArn #-}
{-# DEPRECATED roleAliasArn "Use generic-lens or generic-optics with 'roleAliasArn' instead"  #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleArn :: Lens.Lens' RoleAliasDescription (Core.Maybe Types.RoleArn)
radRoleArn = Lens.field @"roleArn"
{-# INLINEABLE radRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON RoleAliasDescription where
        parseJSON
          = Core.withObject "RoleAliasDescription" Core.$
              \ x ->
                RoleAliasDescription' Core.<$>
                  (x Core..:? "creationDate") Core.<*>
                    x Core..:? "credentialDurationSeconds"
                    Core.<*> x Core..:? "lastModifiedDate"
                    Core.<*> x Core..:? "owner"
                    Core.<*> x Core..:? "roleAlias"
                    Core.<*> x Core..:? "roleAliasArn"
                    Core.<*> x Core..:? "roleArn"
