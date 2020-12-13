{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DeleteStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user or group from an Amazon EMR Studio.
module Network.AWS.EMR.DeleteStudioSessionMapping
  ( -- * Creating a request
    DeleteStudioSessionMapping (..),
    mkDeleteStudioSessionMapping,

    -- ** Request lenses
    dssmStudioId,
    dssmIdentityType,
    dssmIdentityId,
    dssmIdentityName,

    -- * Destructuring the response
    DeleteStudioSessionMappingResponse (..),
    mkDeleteStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStudioSessionMapping' smart constructor.
data DeleteStudioSessionMapping = DeleteStudioSessionMapping'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Lude.Text,
    -- | Specifies whether the identity to delete from the Studio is a user or a group.
    identityType :: IdentityType,
    -- | The globally unique identifier (GUID) of the user or group to remove from the Amazon EMR Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Lude.Maybe Lude.Text,
    -- | The name of the user name or group to remove from the Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStudioSessionMapping' with the minimum fields required to make a request.
--
-- * 'studioId' - The ID of the Amazon EMR Studio.
-- * 'identityType' - Specifies whether the identity to delete from the Studio is a user or a group.
-- * 'identityId' - The globally unique identifier (GUID) of the user or group to remove from the Amazon EMR Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'identityName' - The name of the user name or group to remove from the Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
mkDeleteStudioSessionMapping ::
  -- | 'studioId'
  Lude.Text ->
  -- | 'identityType'
  IdentityType ->
  DeleteStudioSessionMapping
mkDeleteStudioSessionMapping pStudioId_ pIdentityType_ =
  DeleteStudioSessionMapping'
    { studioId = pStudioId_,
      identityType = pIdentityType_,
      identityId = Lude.Nothing,
      identityName = Lude.Nothing
    }

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssmStudioId :: Lens.Lens' DeleteStudioSessionMapping Lude.Text
dssmStudioId = Lens.lens (studioId :: DeleteStudioSessionMapping -> Lude.Text) (\s a -> s {studioId = a} :: DeleteStudioSessionMapping)
{-# DEPRECATED dssmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity to delete from the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssmIdentityType :: Lens.Lens' DeleteStudioSessionMapping IdentityType
dssmIdentityType = Lens.lens (identityType :: DeleteStudioSessionMapping -> IdentityType) (\s a -> s {identityType = a} :: DeleteStudioSessionMapping)
{-# DEPRECATED dssmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The globally unique identifier (GUID) of the user or group to remove from the Amazon EMR Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssmIdentityId :: Lens.Lens' DeleteStudioSessionMapping (Lude.Maybe Lude.Text)
dssmIdentityId = Lens.lens (identityId :: DeleteStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: DeleteStudioSessionMapping)
{-# DEPRECATED dssmIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the user name or group to remove from the Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssmIdentityName :: Lens.Lens' DeleteStudioSessionMapping (Lude.Maybe Lude.Text)
dssmIdentityName = Lens.lens (identityName :: DeleteStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: DeleteStudioSessionMapping)
{-# DEPRECATED dssmIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

instance Lude.AWSRequest DeleteStudioSessionMapping where
  type
    Rs DeleteStudioSessionMapping =
      DeleteStudioSessionMappingResponse
  request = Req.postJSON emrService
  response = Res.receiveNull DeleteStudioSessionMappingResponse'

instance Lude.ToHeaders DeleteStudioSessionMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DeleteStudioSessionMapping" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStudioSessionMapping where
  toJSON DeleteStudioSessionMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StudioId" Lude..= studioId),
            Lude.Just ("IdentityType" Lude..= identityType),
            ("IdentityId" Lude..=) Lude.<$> identityId,
            ("IdentityName" Lude..=) Lude.<$> identityName
          ]
      )

instance Lude.ToPath DeleteStudioSessionMapping where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStudioSessionMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStudioSessionMappingResponse' smart constructor.
data DeleteStudioSessionMappingResponse = DeleteStudioSessionMappingResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStudioSessionMappingResponse' with the minimum fields required to make a request.
mkDeleteStudioSessionMappingResponse ::
  DeleteStudioSessionMappingResponse
mkDeleteStudioSessionMappingResponse =
  DeleteStudioSessionMappingResponse'
