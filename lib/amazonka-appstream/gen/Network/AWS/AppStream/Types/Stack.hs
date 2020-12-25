{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Stack
  ( Stack (..),

    -- * Smart constructor
    mkStack,

    -- * Lenses
    sName,
    sAccessEndpoints,
    sApplicationSettings,
    sArn,
    sCreatedTime,
    sDescription,
    sDisplayName,
    sEmbedHostDomains,
    sFeedbackURL,
    sRedirectURL,
    sStackErrors,
    sStorageConnectors,
    sUserSettings,
  )
where

import qualified Network.AWS.AppStream.Types.AccessEndpoint as Types
import qualified Network.AWS.AppStream.Types.ApplicationSettingsResponse as Types
import qualified Network.AWS.AppStream.Types.Arn as Types
import qualified Network.AWS.AppStream.Types.EmbedHostDomain as Types
import qualified Network.AWS.AppStream.Types.FeedbackURL as Types
import qualified Network.AWS.AppStream.Types.RedirectURL as Types
import qualified Network.AWS.AppStream.Types.StackError as Types
import qualified Network.AWS.AppStream.Types.StorageConnector as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.UserSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a stack.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The name of the stack.
    name :: Types.String,
    -- | The list of virtual private cloud (VPC) interface endpoint objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint),
    -- | The persistent application settings for users of the stack.
    applicationSettings :: Core.Maybe Types.ApplicationSettingsResponse,
    -- | The ARN of the stack.
    arn :: Core.Maybe Types.Arn,
    -- | The time the stack was created.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description to display.
    description :: Core.Maybe Types.String,
    -- | The stack name to display.
    displayName :: Core.Maybe Types.String,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
    embedHostDomains :: Core.Maybe (Core.NonEmpty Types.EmbedHostDomain),
    -- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Types.FeedbackURL,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Core.Maybe Types.RedirectURL,
    -- | The errors for the stack.
    stackErrors :: Core.Maybe [Types.StackError],
    -- | The storage connectors to enable.
    storageConnectors :: Core.Maybe [Types.StorageConnector],
    -- | The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
    userSettings :: Core.Maybe (Core.NonEmpty Types.UserSetting)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Stack' value with any optional fields omitted.
mkStack ::
  -- | 'name'
  Types.String ->
  Stack
mkStack name =
  Stack'
    { name,
      accessEndpoints = Core.Nothing,
      applicationSettings = Core.Nothing,
      arn = Core.Nothing,
      createdTime = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      embedHostDomains = Core.Nothing,
      feedbackURL = Core.Nothing,
      redirectURL = Core.Nothing,
      stackErrors = Core.Nothing,
      storageConnectors = Core.Nothing,
      userSettings = Core.Nothing
    }

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Stack Types.String
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of virtual private cloud (VPC) interface endpoint objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccessEndpoints :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
sAccessEndpoints = Lens.field @"accessEndpoints"
{-# DEPRECATED sAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The persistent application settings for users of the stack.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationSettings :: Lens.Lens' Stack (Core.Maybe Types.ApplicationSettingsResponse)
sApplicationSettings = Lens.field @"applicationSettings"
{-# DEPRECATED sApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The ARN of the stack.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sArn :: Lens.Lens' Stack (Core.Maybe Types.Arn)
sArn = Lens.field @"arn"
{-# DEPRECATED sArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the stack was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedTime :: Lens.Lens' Stack (Core.Maybe Core.NominalDiffTime)
sCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED sCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Stack (Core.Maybe Types.String)
sDescription = Lens.field @"description"
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDisplayName :: Lens.Lens' Stack (Core.Maybe Types.String)
sDisplayName = Lens.field @"displayName"
{-# DEPRECATED sDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEmbedHostDomains :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty Types.EmbedHostDomain))
sEmbedHostDomains = Lens.field @"embedHostDomains"
{-# DEPRECATED sEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFeedbackURL :: Lens.Lens' Stack (Core.Maybe Types.FeedbackURL)
sFeedbackURL = Lens.field @"feedbackURL"
{-# DEPRECATED sFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRedirectURL :: Lens.Lens' Stack (Core.Maybe Types.RedirectURL)
sRedirectURL = Lens.field @"redirectURL"
{-# DEPRECATED sRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

-- | The errors for the stack.
--
-- /Note:/ Consider using 'stackErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackErrors :: Lens.Lens' Stack (Core.Maybe [Types.StackError])
sStackErrors = Lens.field @"stackErrors"
{-# DEPRECATED sStackErrors "Use generic-lens or generic-optics with 'stackErrors' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStorageConnectors :: Lens.Lens' Stack (Core.Maybe [Types.StorageConnector])
sStorageConnectors = Lens.field @"storageConnectors"
{-# DEPRECATED sStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUserSettings :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty Types.UserSetting))
sUserSettings = Lens.field @"userSettings"
{-# DEPRECATED sUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

instance Core.FromJSON Stack where
  parseJSON =
    Core.withObject "Stack" Core.$
      \x ->
        Stack'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "AccessEndpoints")
          Core.<*> (x Core..:? "ApplicationSettings")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "EmbedHostDomains")
          Core.<*> (x Core..:? "FeedbackURL")
          Core.<*> (x Core..:? "RedirectURL")
          Core.<*> (x Core..:? "StackErrors")
          Core.<*> (x Core..:? "StorageConnectors")
          Core.<*> (x Core..:? "UserSettings")
