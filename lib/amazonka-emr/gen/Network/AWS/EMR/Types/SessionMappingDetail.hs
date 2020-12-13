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
    smdStudioId,
    smdLastModifiedTime,
    smdIdentityType,
    smdIdentityId,
    smdSessionPolicyARN,
    smdIdentityName,
  )
where

import Network.AWS.EMR.Types.IdentityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for an Amazon EMR Studio session mapping including creation time, user or group ID, Studio ID, and so on.
--
-- /See:/ 'mkSessionMappingDetail' smart constructor.
data SessionMappingDetail = SessionMappingDetail'
  { -- | The time the session mapping was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Lude.Maybe Lude.Text,
    -- | The time the session mapping was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Specifies whether the identity mapped to the Studio is a user or a group.
    identityType :: Lude.Maybe IdentityType,
    -- | The globally unique identifier (GUID) of the user or group.
    identityId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
    sessionPolicyARN :: Lude.Maybe Lude.Text,
    -- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
    identityName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SessionMappingDetail' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the session mapping was created.
-- * 'studioId' - The ID of the Amazon EMR Studio.
-- * 'lastModifiedTime' - The time the session mapping was last modified.
-- * 'identityType' - Specifies whether the identity mapped to the Studio is a user or a group.
-- * 'identityId' - The globally unique identifier (GUID) of the user or group.
-- * 'sessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy associated with the user or group.
-- * 'identityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
mkSessionMappingDetail ::
  SessionMappingDetail
mkSessionMappingDetail =
  SessionMappingDetail'
    { creationTime = Lude.Nothing,
      studioId = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      identityType = Lude.Nothing,
      identityId = Lude.Nothing,
      sessionPolicyARN = Lude.Nothing,
      identityName = Lude.Nothing
    }

-- | The time the session mapping was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdCreationTime :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Timestamp)
smdCreationTime = Lens.lens (creationTime :: SessionMappingDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: SessionMappingDetail)
{-# DEPRECATED smdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdStudioId :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Text)
smdStudioId = Lens.lens (studioId :: SessionMappingDetail -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: SessionMappingDetail)
{-# DEPRECATED smdStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The time the session mapping was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdLastModifiedTime :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Timestamp)
smdLastModifiedTime = Lens.lens (lastModifiedTime :: SessionMappingDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: SessionMappingDetail)
{-# DEPRECATED smdLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Specifies whether the identity mapped to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityType :: Lens.Lens' SessionMappingDetail (Lude.Maybe IdentityType)
smdIdentityType = Lens.lens (identityType :: SessionMappingDetail -> Lude.Maybe IdentityType) (\s a -> s {identityType = a} :: SessionMappingDetail)
{-# DEPRECATED smdIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The globally unique identifier (GUID) of the user or group.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityId :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Text)
smdIdentityId = Lens.lens (identityId :: SessionMappingDetail -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: SessionMappingDetail)
{-# DEPRECATED smdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- /Note:/ Consider using 'sessionPolicyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdSessionPolicyARN :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Text)
smdSessionPolicyARN = Lens.lens (sessionPolicyARN :: SessionMappingDetail -> Lude.Maybe Lude.Text) (\s a -> s {sessionPolicyARN = a} :: SessionMappingDetail)
{-# DEPRECATED smdSessionPolicyARN "Use generic-lens or generic-optics with 'sessionPolicyARN' instead." #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smdIdentityName :: Lens.Lens' SessionMappingDetail (Lude.Maybe Lude.Text)
smdIdentityName = Lens.lens (identityName :: SessionMappingDetail -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: SessionMappingDetail)
{-# DEPRECATED smdIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

instance Lude.FromJSON SessionMappingDetail where
  parseJSON =
    Lude.withObject
      "SessionMappingDetail"
      ( \x ->
          SessionMappingDetail'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "StudioId")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "IdentityType")
            Lude.<*> (x Lude..:? "IdentityId")
            Lude.<*> (x Lude..:? "SessionPolicyArn")
            Lude.<*> (x Lude..:? "IdentityName")
      )
