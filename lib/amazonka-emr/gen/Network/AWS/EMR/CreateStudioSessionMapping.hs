{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CreateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps a user or group to the Amazon EMR Studio specified by @StudioId@ , and applies a session policy to refine Studio permissions for that user or group.
module Network.AWS.EMR.CreateStudioSessionMapping
  ( -- * Creating a request
    CreateStudioSessionMapping (..),
    mkCreateStudioSessionMapping,

    -- ** Request lenses
    cssmStudioId,
    cssmIdentityType,
    cssmIdentityId,
    cssmSessionPolicyARN,
    cssmIdentityName,

    -- * Destructuring the response
    CreateStudioSessionMappingResponse (..),
    mkCreateStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStudioSessionMapping' smart constructor.
data CreateStudioSessionMapping = CreateStudioSessionMapping'
  { -- | The ID of the Amazon EMR Studio to which the user or group will be mapped.
    studioId :: Lude.Text,
    -- | Specifies whether the identity to map to the Studio is a user or a group.
    identityType :: IdentityType,
    -- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
    sessionPolicyARN :: Lude.Text,
    -- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudioSessionMapping' with the minimum fields required to make a request.
--
-- * 'studioId' - The ID of the Amazon EMR Studio to which the user or group will be mapped.
-- * 'identityType' - Specifies whether the identity to map to the Studio is a user or a group.
-- * 'identityId' - The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'sessionPolicyARN' - The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
-- * 'identityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
mkCreateStudioSessionMapping ::
  -- | 'studioId'
  Lude.Text ->
  -- | 'identityType'
  IdentityType ->
  -- | 'sessionPolicyARN'
  Lude.Text ->
  CreateStudioSessionMapping
mkCreateStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyARN_ =
    CreateStudioSessionMapping'
      { studioId = pStudioId_,
        identityType = pIdentityType_,
        identityId = Lude.Nothing,
        sessionPolicyARN = pSessionPolicyARN_,
        identityName = Lude.Nothing
      }

-- | The ID of the Amazon EMR Studio to which the user or group will be mapped.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmStudioId :: Lens.Lens' CreateStudioSessionMapping Lude.Text
cssmStudioId = Lens.lens (studioId :: CreateStudioSessionMapping -> Lude.Text) (\s a -> s {studioId = a} :: CreateStudioSessionMapping)
{-# DEPRECATED cssmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity to map to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityType :: Lens.Lens' CreateStudioSessionMapping IdentityType
cssmIdentityType = Lens.lens (identityType :: CreateStudioSessionMapping -> IdentityType) (\s a -> s {identityType = a} :: CreateStudioSessionMapping)
{-# DEPRECATED cssmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityId :: Lens.Lens' CreateStudioSessionMapping (Lude.Maybe Lude.Text)
cssmIdentityId = Lens.lens (identityId :: CreateStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: CreateStudioSessionMapping)
{-# DEPRECATED cssmIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
--
-- /Note:/ Consider using 'sessionPolicyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmSessionPolicyARN :: Lens.Lens' CreateStudioSessionMapping Lude.Text
cssmSessionPolicyARN = Lens.lens (sessionPolicyARN :: CreateStudioSessionMapping -> Lude.Text) (\s a -> s {sessionPolicyARN = a} :: CreateStudioSessionMapping)
{-# DEPRECATED cssmSessionPolicyARN "Use generic-lens or generic-optics with 'sessionPolicyARN' instead." #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityName :: Lens.Lens' CreateStudioSessionMapping (Lude.Maybe Lude.Text)
cssmIdentityName = Lens.lens (identityName :: CreateStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: CreateStudioSessionMapping)
{-# DEPRECATED cssmIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

instance Lude.AWSRequest CreateStudioSessionMapping where
  type
    Rs CreateStudioSessionMapping =
      CreateStudioSessionMappingResponse
  request = Req.postJSON emrService
  response = Res.receiveNull CreateStudioSessionMappingResponse'

instance Lude.ToHeaders CreateStudioSessionMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.CreateStudioSessionMapping" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStudioSessionMapping where
  toJSON CreateStudioSessionMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StudioId" Lude..= studioId),
            Lude.Just ("IdentityType" Lude..= identityType),
            ("IdentityId" Lude..=) Lude.<$> identityId,
            Lude.Just ("SessionPolicyArn" Lude..= sessionPolicyARN),
            ("IdentityName" Lude..=) Lude.<$> identityName
          ]
      )

instance Lude.ToPath CreateStudioSessionMapping where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStudioSessionMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStudioSessionMappingResponse' smart constructor.
data CreateStudioSessionMappingResponse = CreateStudioSessionMappingResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudioSessionMappingResponse' with the minimum fields required to make a request.
mkCreateStudioSessionMappingResponse ::
  CreateStudioSessionMappingResponse
mkCreateStudioSessionMappingResponse =
  CreateStudioSessionMappingResponse'
