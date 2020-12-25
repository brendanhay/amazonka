{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SessionMappingDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingDetail
  ( SessionMappingDetail (..),

    -- * Smart constructor
    mkSessionMappingDetail,

    -- * Lenses
    smdCreationTime,
    smdIdentityId,
    smdIdentityName,
    smdIdentityType,
    smdLastModifiedTime,
    smdSessionPolicyArn,
    smdStudioId,
  )
where

import qualified Network.AWS.EMR.Types.IdentityId as Types
import qualified Network.AWS.EMR.Types.IdentityName as Types
import qualified Network.AWS.EMR.Types.IdentityType as Types
import qualified Network.AWS.EMR.Types.SessionPolicyArn as Types
import qualified Network.AWS.EMR.Types.StudioId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for an Amazon EMR Studio session mapping including creation time, user or group ID, Studio ID, and so on.
--
-- /See:/ 'mkSessionMappingDetail' smart constructor.
data SessionMappingDetail = SessionMappingDetail'
  { -- | The time the session mapping was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The globally unique identifier (GUID) of the user or group.
    identityId :: Core.Maybe Types.IdentityId,
    -- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
    identityName :: Core.Maybe Types.IdentityName,
    -- | Specifies whether the identity mapped to the Studio is a user or a group.
    identityType :: Core.Maybe Types.IdentityType,
    -- | The time the session mapping was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
    sessionPolicyArn :: Core.Maybe Types.SessionPolicyArn,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Types.StudioId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SessionMappingDetail' value with any optional fields omitted.
mkSessionMappingDetail ::
  SessionMappingDetail
mkSessionMappingDetail =
  SessionMappingDetail'
    { creationTime = Core.Nothing,
      identityId = Core.Nothing,
      identityName = Core.Nothing,
      identityType = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      sessionPolicyArn = Core.Nothing,
      studioId = Core.Nothing
    }

-- | The time the session mapping was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdCreationTime :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.NominalDiffTime)
smdCreationTime = Lens.field @"creationTime"
{-# DEPRECATED smdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The globally unique identifier (GUID) of the user or group.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityId :: Lens.Lens' SessionMappingDetail (Core.Maybe Types.IdentityId)
smdIdentityId = Lens.field @"identityId"
{-# DEPRECATED smdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityName :: Lens.Lens' SessionMappingDetail (Core.Maybe Types.IdentityName)
smdIdentityName = Lens.field @"identityName"
{-# DEPRECATED smdIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

-- | Specifies whether the identity mapped to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityType :: Lens.Lens' SessionMappingDetail (Core.Maybe Types.IdentityType)
smdIdentityType = Lens.field @"identityType"
{-# DEPRECATED smdIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The time the session mapping was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdLastModifiedTime :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.NominalDiffTime)
smdLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED smdLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- /Note:/ Consider using 'sessionPolicyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdSessionPolicyArn :: Lens.Lens' SessionMappingDetail (Core.Maybe Types.SessionPolicyArn)
smdSessionPolicyArn = Lens.field @"sessionPolicyArn"
{-# DEPRECATED smdSessionPolicyArn "Use generic-lens or generic-optics with 'sessionPolicyArn' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdStudioId :: Lens.Lens' SessionMappingDetail (Core.Maybe Types.StudioId)
smdStudioId = Lens.field @"studioId"
{-# DEPRECATED smdStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

instance Core.FromJSON SessionMappingDetail where
  parseJSON =
    Core.withObject "SessionMappingDetail" Core.$
      \x ->
        SessionMappingDetail'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "IdentityId")
          Core.<*> (x Core..:? "IdentityName")
          Core.<*> (x Core..:? "IdentityType")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "SessionPolicyArn")
          Core.<*> (x Core..:? "StudioId")
