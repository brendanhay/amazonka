{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches mapping details for the specified Amazon EMR Studio and identity (user or group).
module Network.AWS.EMR.GetStudioSessionMapping
  ( -- * Creating a request
    GetStudioSessionMapping (..),
    mkGetStudioSessionMapping,

    -- ** Request lenses
    gssmIdentityId,
    gssmIdentityName,
    gssmStudioId,
    gssmIdentityType,

    -- * Destructuring the response
    GetStudioSessionMappingResponse (..),
    mkGetStudioSessionMappingResponse,

    -- ** Response lenses
    gssmrsSessionMapping,
    gssmrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetStudioSessionMapping' smart constructor.
data GetStudioSessionMapping = GetStudioSessionMapping'
  { identityId ::
      Lude.Maybe Lude.Text,
    identityName :: Lude.Maybe Lude.Text,
    studioId :: Lude.Text,
    identityType :: IdentityType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStudioSessionMapping' with the minimum fields required to make a request.
--
-- * 'identityId' - The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'identityName' - The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
-- * 'identityType' - Specifies whether the identity to fetch is a user or a group.
-- * 'studioId' - The ID of the Amazon EMR Studio.
mkGetStudioSessionMapping ::
  -- | 'studioId'
  Lude.Text ->
  -- | 'identityType'
  IdentityType ->
  GetStudioSessionMapping
mkGetStudioSessionMapping pStudioId_ pIdentityType_ =
  GetStudioSessionMapping'
    { identityId = Lude.Nothing,
      identityName = Lude.Nothing,
      studioId = pStudioId_,
      identityType = pIdentityType_
    }

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityId :: Lens.Lens' GetStudioSessionMapping (Lude.Maybe Lude.Text)
gssmIdentityId = Lens.lens (identityId :: GetStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetStudioSessionMapping)
{-# DEPRECATED gssmIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityName :: Lens.Lens' GetStudioSessionMapping (Lude.Maybe Lude.Text)
gssmIdentityName = Lens.lens (identityName :: GetStudioSessionMapping -> Lude.Maybe Lude.Text) (\s a -> s {identityName = a} :: GetStudioSessionMapping)
{-# DEPRECATED gssmIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmStudioId :: Lens.Lens' GetStudioSessionMapping Lude.Text
gssmStudioId = Lens.lens (studioId :: GetStudioSessionMapping -> Lude.Text) (\s a -> s {studioId = a} :: GetStudioSessionMapping)
{-# DEPRECATED gssmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity to fetch is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityType :: Lens.Lens' GetStudioSessionMapping IdentityType
gssmIdentityType = Lens.lens (identityType :: GetStudioSessionMapping -> IdentityType) (\s a -> s {identityType = a} :: GetStudioSessionMapping)
{-# DEPRECATED gssmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

instance Lude.AWSRequest GetStudioSessionMapping where
  type Rs GetStudioSessionMapping = GetStudioSessionMappingResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetStudioSessionMappingResponse'
            Lude.<$> (x Lude..?> "SessionMapping")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStudioSessionMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.GetStudioSessionMapping" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetStudioSessionMapping where
  toJSON GetStudioSessionMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdentityId" Lude..=) Lude.<$> identityId,
            ("IdentityName" Lude..=) Lude.<$> identityName,
            Lude.Just ("StudioId" Lude..= studioId),
            Lude.Just ("IdentityType" Lude..= identityType)
          ]
      )

instance Lude.ToPath GetStudioSessionMapping where
  toPath = Lude.const "/"

instance Lude.ToQuery GetStudioSessionMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { sessionMapping ::
      Lude.Maybe
        SessionMappingDetail,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStudioSessionMappingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sessionMapping' - The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
mkGetStudioSessionMappingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStudioSessionMappingResponse
mkGetStudioSessionMappingResponse pResponseStatus_ =
  GetStudioSessionMappingResponse'
    { sessionMapping = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
--
-- /Note:/ Consider using 'sessionMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmrsSessionMapping :: Lens.Lens' GetStudioSessionMappingResponse (Lude.Maybe SessionMappingDetail)
gssmrsSessionMapping = Lens.lens (sessionMapping :: GetStudioSessionMappingResponse -> Lude.Maybe SessionMappingDetail) (\s a -> s {sessionMapping = a} :: GetStudioSessionMappingResponse)
{-# DEPRECATED gssmrsSessionMapping "Use generic-lens or generic-optics with 'sessionMapping' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmrsResponseStatus :: Lens.Lens' GetStudioSessionMappingResponse Lude.Int
gssmrsResponseStatus = Lens.lens (responseStatus :: GetStudioSessionMappingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStudioSessionMappingResponse)
{-# DEPRECATED gssmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
