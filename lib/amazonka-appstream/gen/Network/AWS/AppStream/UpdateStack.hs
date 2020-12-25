{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    usName,
    usAccessEndpoints,
    usApplicationSettings,
    usAttributesToDelete,
    usDeleteStorageConnectors,
    usDescription,
    usDisplayName,
    usEmbedHostDomains,
    usFeedbackURL,
    usRedirectURL,
    usStorageConnectors,
    usUserSettings,

    -- * Destructuring the response
    UpdateStackResponse (..),
    mkUpdateStackResponse,

    -- ** Response lenses
    usrrsStack,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | The name of the stack.
    name :: Types.String,
    -- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint),
    -- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
    applicationSettings :: Core.Maybe Types.ApplicationSettings,
    -- | The stack attributes to delete.
    attributesToDelete :: Core.Maybe [Types.StackAttribute],
    -- | Deletes the storage connectors currently enabled for the stack.
    deleteStorageConnectors :: Core.Maybe Core.Bool,
    -- | The description to display.
    description :: Core.Maybe Types.Description,
    -- | The stack name to display.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
    embedHostDomains :: Core.Maybe (Core.NonEmpty Types.EmbedHostDomain),
    -- | The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Types.FeedbackURL,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Core.Maybe Types.RedirectURL,
    -- | The storage connectors to enable.
    storageConnectors :: Core.Maybe [Types.StorageConnector],
    -- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
    userSettings :: Core.Maybe (Core.NonEmpty Types.UserSetting)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStack' value with any optional fields omitted.
mkUpdateStack ::
  -- | 'name'
  Types.String ->
  UpdateStack
mkUpdateStack name =
  UpdateStack'
    { name,
      accessEndpoints = Core.Nothing,
      applicationSettings = Core.Nothing,
      attributesToDelete = Core.Nothing,
      deleteStorageConnectors = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      embedHostDomains = Core.Nothing,
      feedbackURL = Core.Nothing,
      redirectURL = Core.Nothing,
      storageConnectors = Core.Nothing,
      userSettings = Core.Nothing
    }

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateStack Types.String
usName = Lens.field @"name"
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAccessEndpoints :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
usAccessEndpoints = Lens.field @"accessEndpoints"
{-# DEPRECATED usAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usApplicationSettings :: Lens.Lens' UpdateStack (Core.Maybe Types.ApplicationSettings)
usApplicationSettings = Lens.field @"applicationSettings"
{-# DEPRECATED usApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The stack attributes to delete.
--
-- /Note:/ Consider using 'attributesToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAttributesToDelete :: Lens.Lens' UpdateStack (Core.Maybe [Types.StackAttribute])
usAttributesToDelete = Lens.field @"attributesToDelete"
{-# DEPRECATED usAttributesToDelete "Use generic-lens or generic-optics with 'attributesToDelete' instead." #-}

-- | Deletes the storage connectors currently enabled for the stack.
--
-- /Note:/ Consider using 'deleteStorageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDeleteStorageConnectors :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
usDeleteStorageConnectors = Lens.field @"deleteStorageConnectors"
{-# DEPRECATED usDeleteStorageConnectors "Use generic-lens or generic-optics with 'deleteStorageConnectors' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateStack (Core.Maybe Types.Description)
usDescription = Lens.field @"description"
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDisplayName :: Lens.Lens' UpdateStack (Core.Maybe Types.DisplayName)
usDisplayName = Lens.field @"displayName"
{-# DEPRECATED usDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usEmbedHostDomains :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty Types.EmbedHostDomain))
usEmbedHostDomains = Lens.field @"embedHostDomains"
{-# DEPRECATED usEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usFeedbackURL :: Lens.Lens' UpdateStack (Core.Maybe Types.FeedbackURL)
usFeedbackURL = Lens.field @"feedbackURL"
{-# DEPRECATED usFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRedirectURL :: Lens.Lens' UpdateStack (Core.Maybe Types.RedirectURL)
usRedirectURL = Lens.field @"redirectURL"
{-# DEPRECATED usRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStorageConnectors :: Lens.Lens' UpdateStack (Core.Maybe [Types.StorageConnector])
usStorageConnectors = Lens.field @"storageConnectors"
{-# DEPRECATED usStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUserSettings :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty Types.UserSetting))
usUserSettings = Lens.field @"userSettings"
{-# DEPRECATED usUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

instance Core.FromJSON UpdateStack where
  toJSON UpdateStack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("AccessEndpoints" Core..=) Core.<$> accessEndpoints,
            ("ApplicationSettings" Core..=) Core.<$> applicationSettings,
            ("AttributesToDelete" Core..=) Core.<$> attributesToDelete,
            ("DeleteStorageConnectors" Core..=)
              Core.<$> deleteStorageConnectors,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EmbedHostDomains" Core..=) Core.<$> embedHostDomains,
            ("FeedbackURL" Core..=) Core.<$> feedbackURL,
            ("RedirectURL" Core..=) Core.<$> redirectURL,
            ("StorageConnectors" Core..=) Core.<$> storageConnectors,
            ("UserSettings" Core..=) Core.<$> userSettings
          ]
      )

instance Core.AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.UpdateStack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStackResponse'
            Core.<$> (x Core..:? "Stack") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { -- | Information about the stack.
    stack :: Core.Maybe Types.Stack,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateStackResponse' value with any optional fields omitted.
mkUpdateStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateStackResponse
mkUpdateStackResponse responseStatus =
  UpdateStackResponse' {stack = Core.Nothing, responseStatus}

-- | Information about the stack.
--
-- /Note:/ Consider using 'stack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsStack :: Lens.Lens' UpdateStackResponse (Core.Maybe Types.Stack)
usrrsStack = Lens.field @"stack"
{-# DEPRECATED usrrsStack "Use generic-lens or generic-optics with 'stack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateStackResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
