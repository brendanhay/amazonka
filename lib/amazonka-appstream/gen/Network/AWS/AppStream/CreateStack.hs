{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack to start streaming applications to users. A stack consists of an associated fleet, user access policies, and storage configurations.
module Network.AWS.AppStream.CreateStack
  ( -- * Creating a request
    CreateStack (..),
    mkCreateStack,

    -- ** Request lenses
    csName,
    csAccessEndpoints,
    csApplicationSettings,
    csDescription,
    csDisplayName,
    csEmbedHostDomains,
    csFeedbackURL,
    csRedirectURL,
    csStorageConnectors,
    csTags,
    csUserSettings,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    csrrsStack,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | The name of the stack.
    name :: Types.Name,
    -- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint),
    -- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
    applicationSettings :: Core.Maybe Types.ApplicationSettings,
    -- | The description to display.
    description :: Core.Maybe Types.Description,
    -- | The stack name to display.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
    embedHostDomains :: Core.Maybe (Core.NonEmpty Types.EmbedHostDomain),
    -- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Types.FeedbackURL,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Core.Maybe Types.RedirectURL,
    -- | The storage connectors to enable.
    storageConnectors :: Core.Maybe [Types.StorageConnector],
    -- | The tags to associate with the stack. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
    --
    -- If you do not specify a value, the value is set to an empty string.
    -- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
    -- _ . : / = + \ - @
    -- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
    userSettings :: Core.Maybe (Core.NonEmpty Types.UserSetting)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStack' value with any optional fields omitted.
mkCreateStack ::
  -- | 'name'
  Types.Name ->
  CreateStack
mkCreateStack name =
  CreateStack'
    { name,
      accessEndpoints = Core.Nothing,
      applicationSettings = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      embedHostDomains = Core.Nothing,
      feedbackURL = Core.Nothing,
      redirectURL = Core.Nothing,
      storageConnectors = Core.Nothing,
      tags = Core.Nothing,
      userSettings = Core.Nothing
    }

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateStack Types.Name
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAccessEndpoints :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
csAccessEndpoints = Lens.field @"accessEndpoints"
{-# DEPRECATED csAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csApplicationSettings :: Lens.Lens' CreateStack (Core.Maybe Types.ApplicationSettings)
csApplicationSettings = Lens.field @"applicationSettings"
{-# DEPRECATED csApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateStack (Core.Maybe Types.Description)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisplayName :: Lens.Lens' CreateStack (Core.Maybe Types.DisplayName)
csDisplayName = Lens.field @"displayName"
{-# DEPRECATED csDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEmbedHostDomains :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty Types.EmbedHostDomain))
csEmbedHostDomains = Lens.field @"embedHostDomains"
{-# DEPRECATED csEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csFeedbackURL :: Lens.Lens' CreateStack (Core.Maybe Types.FeedbackURL)
csFeedbackURL = Lens.field @"feedbackURL"
{-# DEPRECATED csFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRedirectURL :: Lens.Lens' CreateStack (Core.Maybe Types.RedirectURL)
csRedirectURL = Lens.field @"redirectURL"
{-# DEPRECATED csRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStorageConnectors :: Lens.Lens' CreateStack (Core.Maybe [Types.StorageConnector])
csStorageConnectors = Lens.field @"storageConnectors"
{-# DEPRECATED csStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The tags to associate with the stack. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStack (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUserSettings :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty Types.UserSetting))
csUserSettings = Lens.field @"userSettings"
{-# DEPRECATED csUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

instance Core.FromJSON CreateStack where
  toJSON CreateStack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("AccessEndpoints" Core..=) Core.<$> accessEndpoints,
            ("ApplicationSettings" Core..=) Core.<$> applicationSettings,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EmbedHostDomains" Core..=) Core.<$> embedHostDomains,
            ("FeedbackURL" Core..=) Core.<$> feedbackURL,
            ("RedirectURL" Core..=) Core.<$> redirectURL,
            ("StorageConnectors" Core..=) Core.<$> storageConnectors,
            ("Tags" Core..=) Core.<$> tags,
            ("UserSettings" Core..=) Core.<$> userSettings
          ]
      )

instance Core.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.CreateStack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Core.<$> (x Core..:? "Stack") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Information about the stack.
    stack :: Core.Maybe Types.Stack,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateStackResponse' value with any optional fields omitted.
mkCreateStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStackResponse
mkCreateStackResponse responseStatus =
  CreateStackResponse' {stack = Core.Nothing, responseStatus}

-- | Information about the stack.
--
-- /Note:/ Consider using 'stack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStack :: Lens.Lens' CreateStackResponse (Core.Maybe Types.Stack)
csrrsStack = Lens.field @"stack"
{-# DEPRECATED csrrsStack "Use generic-lens or generic-optics with 'stack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStackResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
