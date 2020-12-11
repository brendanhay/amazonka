{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.RemoveResourcePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the permission for the specified principal from the specified resource.
module Network.AWS.WorkDocs.RemoveResourcePermission
  ( -- * Creating a request
    RemoveResourcePermission (..),
    mkRemoveResourcePermission,

    -- ** Request lenses
    rrpPrincipalType,
    rrpAuthenticationToken,
    rrpResourceId,
    rrpPrincipalId,

    -- * Destructuring the response
    RemoveResourcePermissionResponse (..),
    mkRemoveResourcePermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkRemoveResourcePermission' smart constructor.
data RemoveResourcePermission = RemoveResourcePermission'
  { principalType ::
      Lude.Maybe PrincipalType,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    resourceId :: Lude.Text,
    principalId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveResourcePermission' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'principalId' - The principal ID of the resource.
-- * 'principalType' - The principal type of the resource.
-- * 'resourceId' - The ID of the resource.
mkRemoveResourcePermission ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'principalId'
  Lude.Text ->
  RemoveResourcePermission
mkRemoveResourcePermission pResourceId_ pPrincipalId_ =
  RemoveResourcePermission'
    { principalType = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      resourceId = pResourceId_,
      principalId = pPrincipalId_
    }

-- | The principal type of the resource.
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpPrincipalType :: Lens.Lens' RemoveResourcePermission (Lude.Maybe PrincipalType)
rrpPrincipalType = Lens.lens (principalType :: RemoveResourcePermission -> Lude.Maybe PrincipalType) (\s a -> s {principalType = a} :: RemoveResourcePermission)
{-# DEPRECATED rrpPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpAuthenticationToken :: Lens.Lens' RemoveResourcePermission (Lude.Maybe (Lude.Sensitive Lude.Text))
rrpAuthenticationToken = Lens.lens (authenticationToken :: RemoveResourcePermission -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: RemoveResourcePermission)
{-# DEPRECATED rrpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpResourceId :: Lens.Lens' RemoveResourcePermission Lude.Text
rrpResourceId = Lens.lens (resourceId :: RemoveResourcePermission -> Lude.Text) (\s a -> s {resourceId = a} :: RemoveResourcePermission)
{-# DEPRECATED rrpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The principal ID of the resource.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpPrincipalId :: Lens.Lens' RemoveResourcePermission Lude.Text
rrpPrincipalId = Lens.lens (principalId :: RemoveResourcePermission -> Lude.Text) (\s a -> s {principalId = a} :: RemoveResourcePermission)
{-# DEPRECATED rrpPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

instance Lude.AWSRequest RemoveResourcePermission where
  type Rs RemoveResourcePermission = RemoveResourcePermissionResponse
  request = Req.delete workDocsService
  response = Res.receiveNull RemoveResourcePermissionResponse'

instance Lude.ToHeaders RemoveResourcePermission where
  toHeaders RemoveResourcePermission' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath RemoveResourcePermission where
  toPath RemoveResourcePermission' {..} =
    Lude.mconcat
      [ "/api/v1/resources/",
        Lude.toBS resourceId,
        "/permissions/",
        Lude.toBS principalId
      ]

instance Lude.ToQuery RemoveResourcePermission where
  toQuery RemoveResourcePermission' {..} =
    Lude.mconcat ["type" Lude.=: principalType]

-- | /See:/ 'mkRemoveResourcePermissionResponse' smart constructor.
data RemoveResourcePermissionResponse = RemoveResourcePermissionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveResourcePermissionResponse' with the minimum fields required to make a request.
mkRemoveResourcePermissionResponse ::
  RemoveResourcePermissionResponse
mkRemoveResourcePermissionResponse =
  RemoveResourcePermissionResponse'
