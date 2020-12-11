{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.AddResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.
module Network.AWS.WorkDocs.AddResourcePermissions
  ( -- * Creating a request
    AddResourcePermissions (..),
    mkAddResourcePermissions,

    -- ** Request lenses
    arpNotificationOptions,
    arpAuthenticationToken,
    arpResourceId,
    arpPrincipals,

    -- * Destructuring the response
    AddResourcePermissionsResponse (..),
    mkAddResourcePermissionsResponse,

    -- ** Response lenses
    arprsShareResults,
    arprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkAddResourcePermissions' smart constructor.
data AddResourcePermissions = AddResourcePermissions'
  { notificationOptions ::
      Lude.Maybe NotificationOptions,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    resourceId :: Lude.Text,
    principals :: [SharePrincipal]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddResourcePermissions' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'notificationOptions' - The notification options.
-- * 'principals' - The users, groups, or organization being granted permission.
-- * 'resourceId' - The ID of the resource.
mkAddResourcePermissions ::
  -- | 'resourceId'
  Lude.Text ->
  AddResourcePermissions
mkAddResourcePermissions pResourceId_ =
  AddResourcePermissions'
    { notificationOptions = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      resourceId = pResourceId_,
      principals = Lude.mempty
    }

-- | The notification options.
--
-- /Note:/ Consider using 'notificationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpNotificationOptions :: Lens.Lens' AddResourcePermissions (Lude.Maybe NotificationOptions)
arpNotificationOptions = Lens.lens (notificationOptions :: AddResourcePermissions -> Lude.Maybe NotificationOptions) (\s a -> s {notificationOptions = a} :: AddResourcePermissions)
{-# DEPRECATED arpNotificationOptions "Use generic-lens or generic-optics with 'notificationOptions' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpAuthenticationToken :: Lens.Lens' AddResourcePermissions (Lude.Maybe (Lude.Sensitive Lude.Text))
arpAuthenticationToken = Lens.lens (authenticationToken :: AddResourcePermissions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: AddResourcePermissions)
{-# DEPRECATED arpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpResourceId :: Lens.Lens' AddResourcePermissions Lude.Text
arpResourceId = Lens.lens (resourceId :: AddResourcePermissions -> Lude.Text) (\s a -> s {resourceId = a} :: AddResourcePermissions)
{-# DEPRECATED arpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The users, groups, or organization being granted permission.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpPrincipals :: Lens.Lens' AddResourcePermissions [SharePrincipal]
arpPrincipals = Lens.lens (principals :: AddResourcePermissions -> [SharePrincipal]) (\s a -> s {principals = a} :: AddResourcePermissions)
{-# DEPRECATED arpPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

instance Lude.AWSRequest AddResourcePermissions where
  type Rs AddResourcePermissions = AddResourcePermissionsResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddResourcePermissionsResponse'
            Lude.<$> (x Lude..?> "ShareResults" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddResourcePermissions where
  toHeaders AddResourcePermissions' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON AddResourcePermissions where
  toJSON AddResourcePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationOptions" Lude..=) Lude.<$> notificationOptions,
            Lude.Just ("Principals" Lude..= principals)
          ]
      )

instance Lude.ToPath AddResourcePermissions where
  toPath AddResourcePermissions' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/permissions"]

instance Lude.ToQuery AddResourcePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddResourcePermissionsResponse' smart constructor.
data AddResourcePermissionsResponse = AddResourcePermissionsResponse'
  { shareResults ::
      Lude.Maybe [ShareResult],
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddResourcePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'shareResults' - The share results.
mkAddResourcePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddResourcePermissionsResponse
mkAddResourcePermissionsResponse pResponseStatus_ =
  AddResourcePermissionsResponse'
    { shareResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The share results.
--
-- /Note:/ Consider using 'shareResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arprsShareResults :: Lens.Lens' AddResourcePermissionsResponse (Lude.Maybe [ShareResult])
arprsShareResults = Lens.lens (shareResults :: AddResourcePermissionsResponse -> Lude.Maybe [ShareResult]) (\s a -> s {shareResults = a} :: AddResourcePermissionsResponse)
{-# DEPRECATED arprsShareResults "Use generic-lens or generic-optics with 'shareResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arprsResponseStatus :: Lens.Lens' AddResourcePermissionsResponse Lude.Int
arprsResponseStatus = Lens.lens (responseStatus :: AddResourcePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddResourcePermissionsResponse)
{-# DEPRECATED arprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
