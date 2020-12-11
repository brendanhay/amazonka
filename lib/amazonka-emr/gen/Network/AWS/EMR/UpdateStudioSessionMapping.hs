{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.UpdateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the session policy attached to the user or group for the specified Amazon EMR Studio.
module Network.AWS.EMR.UpdateStudioSessionMapping
  ( -- * Creating a request
    UpdateStudioSessionMapping (..),
    mkUpdateStudioSessionMapping,

    -- ** Request lenses
    ussmIdentityId,
    ussmIdentityName,
    ussmStudioId,
    ussmIdentityType,
    ussmSessionPolicyARN,

    -- * Destructuring the response
    UpdateStudioSessionMappingResponse (..),
    mkUpdateStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStudioSessionMapping' smart constructor.
data UpdateStudioSessionMapping = UpdateStudioSessionMapping'
  { identityId ::
      Lude.Maybe Lude.Text,
    identityName :: Lude.Maybe Lude.Text,
    studioId :: Lude.Text,
    identityType :: IdentityType,
    sessionPolicyARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStudioSessionMapping' with the minimum fields required to make a request.
--
-- * 'identityId' - The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'identityName' - The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'identityType' - Specifies whether the identity to update is a user or a group.
-- * 'sessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
-- * 'studioId' - The ID of the EMR Studio.
mkUpdateStudioSessionMapping ::
  -- | 'studioId'
  Lude.Text ->
  -- | 'identityType'
  IdentityType ->
  -- | 'sessionPolicyARN'
  Lude.Text ->
  UpdateStudioSessionMapping
mkUpdateStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyARN_ =
    UpdateStudioSessionMapping'
      { identityId = Lude.Nothing,
        identityName = Lude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_,
        sessionPolicyARN = pSessionPolicyARN_
      }

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityId :: Lens.Lens' UpdateStudioSessionMapping (Lude.Maybe Lude.Text)
ussmIdentityId = Lens.lens (identityId :: UpdateStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: UpdateStudioSessionMapping)
{-# DEPRECATED ussmIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityName :: Lens.Lens' UpdateStudioSessionMapping (Lude.Maybe Lude.Text)
ussmIdentityName = Lens.lens (identityName :: UpdateStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: UpdateStudioSessionMapping)
{-# DEPRECATED ussmIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

-- | The ID of the EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmStudioId :: Lens.Lens' UpdateStudioSessionMapping Lude.Text
ussmStudioId = Lens.lens (studioId :: UpdateStudioSessionMapping -> Lude.Text) (\s a -> s {studioId = a} :: UpdateStudioSessionMapping)
{-# DEPRECATED ussmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity to update is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityType :: Lens.Lens' UpdateStudioSessionMapping IdentityType
ussmIdentityType = Lens.lens (identityType :: UpdateStudioSessionMapping -> IdentityType) (\s a -> s {identityType = a} :: UpdateStudioSessionMapping)
{-# DEPRECATED ussmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
--
-- /Note:/ Consider using 'sessionPolicyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmSessionPolicyARN :: Lens.Lens' UpdateStudioSessionMapping Lude.Text
ussmSessionPolicyARN = Lens.lens (sessionPolicyARN :: UpdateStudioSessionMapping -> Lude.Text) (\s a -> s {sessionPolicyARN = a} :: UpdateStudioSessionMapping)
{-# DEPRECATED ussmSessionPolicyARN "Use generic-lens or generic-optics with 'sessionPolicyARN' instead." #-}

instance Lude.AWSRequest UpdateStudioSessionMapping where
  type
    Rs UpdateStudioSessionMapping =
      UpdateStudioSessionMappingResponse
  request = Req.postJSON emrService
  response = Res.receiveNull UpdateStudioSessionMappingResponse'

instance Lude.ToHeaders UpdateStudioSessionMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.UpdateStudioSessionMapping" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateStudioSessionMapping where
  toJSON UpdateStudioSessionMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdentityId" Lude..=) Lude.<$> identityId,
            ("IdentityName" Lude..=) Lude.<$> identityName,
            Lude.Just ("StudioId" Lude..= studioId),
            Lude.Just ("IdentityType" Lude..= identityType),
            Lude.Just ("SessionPolicyArn" Lude..= sessionPolicyARN)
          ]
      )

instance Lude.ToPath UpdateStudioSessionMapping where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStudioSessionMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStudioSessionMappingResponse' smart constructor.
data UpdateStudioSessionMappingResponse = UpdateStudioSessionMappingResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStudioSessionMappingResponse' with the minimum fields required to make a request.
mkUpdateStudioSessionMappingResponse ::
  UpdateStudioSessionMappingResponse
mkUpdateStudioSessionMappingResponse =
  UpdateStudioSessionMappingResponse'
