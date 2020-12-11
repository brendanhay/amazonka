-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SessionMappingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingSummary
  ( SessionMappingSummary (..),

    -- * Smart constructor
    mkSessionMappingSummary,

    -- * Lenses
    smsCreationTime,
    smsStudioId,
    smsIdentityType,
    smsIdentityId,
    smsSessionPolicyARN,
    smsIdentityName,
  )
where

import Network.AWS.EMR.Types.IdentityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for an Amazon EMR Studio session mapping. The details do not include the time the session mapping was last modified.
--
-- /See:/ 'mkSessionMappingSummary' smart constructor.
data SessionMappingSummary = SessionMappingSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    studioId :: Lude.Maybe Lude.Text,
    identityType :: Lude.Maybe IdentityType,
    identityId :: Lude.Maybe Lude.Text,
    sessionPolicyARN :: Lude.Maybe Lude.Text,
    identityName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SessionMappingSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the session mapping was created.
-- * 'identityId' - The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
-- * 'identityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
-- * 'identityType' - Specifies whether the identity mapped to the Studio is a user or a group.
-- * 'sessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy associated with the user or group.
-- * 'studioId' - The ID of the Amazon EMR Studio.
mkSessionMappingSummary ::
  SessionMappingSummary
mkSessionMappingSummary =
  SessionMappingSummary'
    { creationTime = Lude.Nothing,
      studioId = Lude.Nothing,
      identityType = Lude.Nothing,
      identityId = Lude.Nothing,
      sessionPolicyARN = Lude.Nothing,
      identityName = Lude.Nothing
    }

-- | The time the session mapping was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsCreationTime :: Lens.Lens' SessionMappingSummary (Lude.Maybe Lude.Timestamp)
smsCreationTime = Lens.lens (creationTime :: SessionMappingSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: SessionMappingSummary)
{-# DEPRECATED smsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsStudioId :: Lens.Lens' SessionMappingSummary (Lude.Maybe Lude.Text)
smsStudioId = Lens.lens (studioId :: SessionMappingSummary -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: SessionMappingSummary)
{-# DEPRECATED smsStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity mapped to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityType :: Lens.Lens' SessionMappingSummary (Lude.Maybe IdentityType)
smsIdentityType = Lens.lens (identityType :: SessionMappingSummary -> Lude.Maybe IdentityType) (\s a -> s {identityType = a} :: SessionMappingSummary)
{-# DEPRECATED smsIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityId :: Lens.Lens' SessionMappingSummary (Lude.Maybe Lude.Text)
smsIdentityId = Lens.lens (identityId :: SessionMappingSummary -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: SessionMappingSummary)
{-# DEPRECATED smsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- /Note:/ Consider using 'sessionPolicyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsSessionPolicyARN :: Lens.Lens' SessionMappingSummary (Lude.Maybe Lude.Text)
smsSessionPolicyARN = Lens.lens (sessionPolicyARN :: SessionMappingSummary -> Lude.Maybe Lude.Text) (\s a -> s {sessionPolicyARN = a} :: SessionMappingSummary)
{-# DEPRECATED smsSessionPolicyARN "Use generic-lens or generic-optics with 'sessionPolicyARN' instead." #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsIdentityName :: Lens.Lens' SessionMappingSummary (Lude.Maybe Lude.Text)
smsIdentityName = Lens.lens (identityName :: SessionMappingSummary -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: SessionMappingSummary)
{-# DEPRECATED smsIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

instance Lude.FromJSON SessionMappingSummary where
  parseJSON =
    Lude.withObject
      "SessionMappingSummary"
      ( \x ->
          SessionMappingSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "StudioId")
            Lude.<*> (x Lude..:? "IdentityType")
            Lude.<*> (x Lude..:? "IdentityId")
            Lude.<*> (x Lude..:? "SessionPolicyArn")
            Lude.<*> (x Lude..:? "IdentityName")
      )
