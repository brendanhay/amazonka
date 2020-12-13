{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.RemoveAllResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all the permissions from the specified resource.
module Network.AWS.WorkDocs.RemoveAllResourcePermissions
  ( -- * Creating a request
    RemoveAllResourcePermissions (..),
    mkRemoveAllResourcePermissions,

    -- ** Request lenses
    rarpResourceId,
    rarpAuthenticationToken,

    -- * Destructuring the response
    RemoveAllResourcePermissionsResponse (..),
    mkRemoveAllResourcePermissionsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkRemoveAllResourcePermissions' smart constructor.
data RemoveAllResourcePermissions = RemoveAllResourcePermissions'
  { -- | The ID of the resource.
    resourceId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAllResourcePermissions' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
mkRemoveAllResourcePermissions ::
  -- | 'resourceId'
  Lude.Text ->
  RemoveAllResourcePermissions
mkRemoveAllResourcePermissions pResourceId_ =
  RemoveAllResourcePermissions'
    { resourceId = pResourceId_,
      authenticationToken = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarpResourceId :: Lens.Lens' RemoveAllResourcePermissions Lude.Text
rarpResourceId = Lens.lens (resourceId :: RemoveAllResourcePermissions -> Lude.Text) (\s a -> s {resourceId = a} :: RemoveAllResourcePermissions)
{-# DEPRECATED rarpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarpAuthenticationToken :: Lens.Lens' RemoveAllResourcePermissions (Lude.Maybe (Lude.Sensitive Lude.Text))
rarpAuthenticationToken = Lens.lens (authenticationToken :: RemoveAllResourcePermissions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: RemoveAllResourcePermissions)
{-# DEPRECATED rarpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Lude.AWSRequest RemoveAllResourcePermissions where
  type
    Rs RemoveAllResourcePermissions =
      RemoveAllResourcePermissionsResponse
  request = Req.delete workDocsService
  response = Res.receiveNull RemoveAllResourcePermissionsResponse'

instance Lude.ToHeaders RemoveAllResourcePermissions where
  toHeaders RemoveAllResourcePermissions' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath RemoveAllResourcePermissions where
  toPath RemoveAllResourcePermissions' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/permissions"]

instance Lude.ToQuery RemoveAllResourcePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveAllResourcePermissionsResponse' smart constructor.
data RemoveAllResourcePermissionsResponse = RemoveAllResourcePermissionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAllResourcePermissionsResponse' with the minimum fields required to make a request.
mkRemoveAllResourcePermissionsResponse ::
  RemoveAllResourcePermissionsResponse
mkRemoveAllResourcePermissionsResponse =
  RemoveAllResourcePermissionsResponse'
