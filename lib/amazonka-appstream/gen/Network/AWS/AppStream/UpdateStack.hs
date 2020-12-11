{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fields for the specified stack.
module Network.AWS.AppStream.UpdateStack
  ( -- * Creating a request
    UpdateStack (..),
    mkUpdateStack,

    -- ** Request lenses
    usUserSettings,
    usApplicationSettings,
    usFeedbackURL,
    usAttributesToDelete,
    usDeleteStorageConnectors,
    usStorageConnectors,
    usAccessEndpoints,
    usDisplayName,
    usEmbedHostDomains,
    usDescription,
    usRedirectURL,
    usName,

    -- * Destructuring the response
    UpdateStackResponse (..),
    mkUpdateStackResponse,

    -- ** Response lenses
    usrsStack,
    usrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { userSettings ::
      Lude.Maybe (Lude.NonEmpty UserSetting),
    applicationSettings :: Lude.Maybe ApplicationSettings,
    feedbackURL :: Lude.Maybe Lude.Text,
    attributesToDelete :: Lude.Maybe [StackAttribute],
    deleteStorageConnectors :: Lude.Maybe Lude.Bool,
    storageConnectors :: Lude.Maybe [StorageConnector],
    accessEndpoints :: Lude.Maybe (Lude.NonEmpty AccessEndpoint),
    displayName :: Lude.Maybe Lude.Text,
    embedHostDomains :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    description :: Lude.Maybe Lude.Text,
    redirectURL :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- * 'accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
-- * 'applicationSettings' - The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
-- * 'attributesToDelete' - The stack attributes to delete.
-- * 'deleteStorageConnectors' - Deletes the storage connectors currently enabled for the stack.
-- * 'description' - The description to display.
-- * 'displayName' - The stack name to display.
-- * 'embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
-- * 'feedbackURL' - The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
-- * 'name' - The name of the stack.
-- * 'redirectURL' - The URL that users are redirected to after their streaming session ends.
-- * 'storageConnectors' - The storage connectors to enable.
-- * 'userSettings' - The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
mkUpdateStack ::
  -- | 'name'
  Lude.Text ->
  UpdateStack
mkUpdateStack pName_ =
  UpdateStack'
    { userSettings = Lude.Nothing,
      applicationSettings = Lude.Nothing,
      feedbackURL = Lude.Nothing,
      attributesToDelete = Lude.Nothing,
      deleteStorageConnectors = Lude.Nothing,
      storageConnectors = Lude.Nothing,
      accessEndpoints = Lude.Nothing,
      displayName = Lude.Nothing,
      embedHostDomains = Lude.Nothing,
      description = Lude.Nothing,
      redirectURL = Lude.Nothing,
      name = pName_
    }

-- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUserSettings :: Lens.Lens' UpdateStack (Lude.Maybe (Lude.NonEmpty UserSetting))
usUserSettings = Lens.lens (userSettings :: UpdateStack -> Lude.Maybe (Lude.NonEmpty UserSetting)) (\s a -> s {userSettings = a} :: UpdateStack)
{-# DEPRECATED usUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usApplicationSettings :: Lens.Lens' UpdateStack (Lude.Maybe ApplicationSettings)
usApplicationSettings = Lens.lens (applicationSettings :: UpdateStack -> Lude.Maybe ApplicationSettings) (\s a -> s {applicationSettings = a} :: UpdateStack)
{-# DEPRECATED usApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usFeedbackURL :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usFeedbackURL = Lens.lens (feedbackURL :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {feedbackURL = a} :: UpdateStack)
{-# DEPRECATED usFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The stack attributes to delete.
--
-- /Note:/ Consider using 'attributesToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAttributesToDelete :: Lens.Lens' UpdateStack (Lude.Maybe [StackAttribute])
usAttributesToDelete = Lens.lens (attributesToDelete :: UpdateStack -> Lude.Maybe [StackAttribute]) (\s a -> s {attributesToDelete = a} :: UpdateStack)
{-# DEPRECATED usAttributesToDelete "Use generic-lens or generic-optics with 'attributesToDelete' instead." #-}

-- | Deletes the storage connectors currently enabled for the stack.
--
-- /Note:/ Consider using 'deleteStorageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDeleteStorageConnectors :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Bool)
usDeleteStorageConnectors = Lens.lens (deleteStorageConnectors :: UpdateStack -> Lude.Maybe Lude.Bool) (\s a -> s {deleteStorageConnectors = a} :: UpdateStack)
{-# DEPRECATED usDeleteStorageConnectors "Use generic-lens or generic-optics with 'deleteStorageConnectors' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStorageConnectors :: Lens.Lens' UpdateStack (Lude.Maybe [StorageConnector])
usStorageConnectors = Lens.lens (storageConnectors :: UpdateStack -> Lude.Maybe [StorageConnector]) (\s a -> s {storageConnectors = a} :: UpdateStack)
{-# DEPRECATED usStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAccessEndpoints :: Lens.Lens' UpdateStack (Lude.Maybe (Lude.NonEmpty AccessEndpoint))
usAccessEndpoints = Lens.lens (accessEndpoints :: UpdateStack -> Lude.Maybe (Lude.NonEmpty AccessEndpoint)) (\s a -> s {accessEndpoints = a} :: UpdateStack)
{-# DEPRECATED usAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDisplayName :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDisplayName = Lens.lens (displayName :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateStack)
{-# DEPRECATED usDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usEmbedHostDomains :: Lens.Lens' UpdateStack (Lude.Maybe (Lude.NonEmpty Lude.Text))
usEmbedHostDomains = Lens.lens (embedHostDomains :: UpdateStack -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {embedHostDomains = a} :: UpdateStack)
{-# DEPRECATED usEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDescription = Lens.lens (description :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateStack)
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRedirectURL :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usRedirectURL = Lens.lens (redirectURL :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {redirectURL = a} :: UpdateStack)
{-# DEPRECATED usRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateStack Lude.Text
usName = Lens.lens (name :: UpdateStack -> Lude.Text) (\s a -> s {name = a} :: UpdateStack)
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateStackResponse'
            Lude.<$> (x Lude..?> "Stack") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.UpdateStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserSettings" Lude..=) Lude.<$> userSettings,
            ("ApplicationSettings" Lude..=) Lude.<$> applicationSettings,
            ("FeedbackURL" Lude..=) Lude.<$> feedbackURL,
            ("AttributesToDelete" Lude..=) Lude.<$> attributesToDelete,
            ("DeleteStorageConnectors" Lude..=)
              Lude.<$> deleteStorageConnectors,
            ("StorageConnectors" Lude..=) Lude.<$> storageConnectors,
            ("AccessEndpoints" Lude..=) Lude.<$> accessEndpoints,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("EmbedHostDomains" Lude..=) Lude.<$> embedHostDomains,
            ("Description" Lude..=) Lude.<$> description,
            ("RedirectURL" Lude..=) Lude.<$> redirectURL,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { stack ::
      Lude.Maybe Stack,
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

-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stack' - Information about the stack.
mkUpdateStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStackResponse
mkUpdateStackResponse pResponseStatus_ =
  UpdateStackResponse'
    { stack = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the stack.
--
-- /Note:/ Consider using 'stack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsStack :: Lens.Lens' UpdateStackResponse (Lude.Maybe Stack)
usrsStack = Lens.lens (stack :: UpdateStackResponse -> Lude.Maybe Stack) (\s a -> s {stack = a} :: UpdateStackResponse)
{-# DEPRECATED usrsStack "Use generic-lens or generic-optics with 'stack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateStackResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStackResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
