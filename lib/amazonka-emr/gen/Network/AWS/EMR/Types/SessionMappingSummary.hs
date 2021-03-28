{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SessionMappingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SessionMappingSummary
  ( SessionMappingSummary (..)
  -- * Smart constructor
  , mkSessionMappingSummary
  -- * Lenses
  , smsCreationTime
  , smsIdentityId
  , smsIdentityName
  , smsIdentityType
  , smsSessionPolicyArn
  , smsStudioId
  ) where

import qualified Network.AWS.EMR.Types.IdentityId as Types
import qualified Network.AWS.EMR.Types.IdentityName as Types
import qualified Network.AWS.EMR.Types.IdentityType as Types
import qualified Network.AWS.EMR.Types.SessionPolicyArn as Types
import qualified Network.AWS.EMR.Types.StudioId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for an Amazon EMR Studio session mapping. The details do not include the time the session mapping was last modified.
--
-- /See:/ 'mkSessionMappingSummary' smart constructor.
data SessionMappingSummary = SessionMappingSummary'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the session mapping was created.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
  , identityName :: Core.Maybe Types.IdentityName
    -- ^ The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
  , identityType :: Core.Maybe Types.IdentityType
    -- ^ Specifies whether the identity mapped to the Studio is a user or a group.
  , sessionPolicyArn :: Core.Maybe Types.SessionPolicyArn
    -- ^ The Amazon Resource Name (ARN) of the session policy associated with the user or group.
  , studioId :: Core.Maybe Types.StudioId
    -- ^ The ID of the Amazon EMR Studio.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SessionMappingSummary' value with any optional fields omitted.
mkSessionMappingSummary
    :: SessionMappingSummary
mkSessionMappingSummary
  = SessionMappingSummary'{creationTime = Core.Nothing,
                           identityId = Core.Nothing, identityName = Core.Nothing,
                           identityType = Core.Nothing, sessionPolicyArn = Core.Nothing,
                           studioId = Core.Nothing}

-- | The time the session mapping was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsCreationTime :: Lens.Lens' SessionMappingSummary (Core.Maybe Core.NominalDiffTime)
smsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE smsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityId :: Lens.Lens' SessionMappingSummary (Core.Maybe Types.IdentityId)
smsIdentityId = Lens.field @"identityId"
{-# INLINEABLE smsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityName :: Lens.Lens' SessionMappingSummary (Core.Maybe Types.IdentityName)
smsIdentityName = Lens.field @"identityName"
{-# INLINEABLE smsIdentityName #-}
{-# DEPRECATED identityName "Use generic-lens or generic-optics with 'identityName' instead"  #-}

-- | Specifies whether the identity mapped to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityType :: Lens.Lens' SessionMappingSummary (Core.Maybe Types.IdentityType)
smsIdentityType = Lens.field @"identityType"
{-# INLINEABLE smsIdentityType #-}
{-# DEPRECATED identityType "Use generic-lens or generic-optics with 'identityType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- /Note:/ Consider using 'sessionPolicyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsSessionPolicyArn :: Lens.Lens' SessionMappingSummary (Core.Maybe Types.SessionPolicyArn)
smsSessionPolicyArn = Lens.field @"sessionPolicyArn"
{-# INLINEABLE smsSessionPolicyArn #-}
{-# DEPRECATED sessionPolicyArn "Use generic-lens or generic-optics with 'sessionPolicyArn' instead"  #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsStudioId :: Lens.Lens' SessionMappingSummary (Core.Maybe Types.StudioId)
smsStudioId = Lens.field @"studioId"
{-# INLINEABLE smsStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

instance Core.FromJSON SessionMappingSummary where
        parseJSON
          = Core.withObject "SessionMappingSummary" Core.$
              \ x ->
                SessionMappingSummary' Core.<$>
                  (x Core..:? "CreationTime") Core.<*> x Core..:? "IdentityId"
                    Core.<*> x Core..:? "IdentityName"
                    Core.<*> x Core..:? "IdentityType"
                    Core.<*> x Core..:? "SessionPolicyArn"
                    Core.<*> x Core..:? "StudioId"
