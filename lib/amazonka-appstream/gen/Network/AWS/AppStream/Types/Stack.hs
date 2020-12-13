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
    sUserSettings,
    sApplicationSettings,
    sFeedbackURL,
    sARN,
    sCreatedTime,
    sStorageConnectors,
    sAccessEndpoints,
    sName,
    sDisplayName,
    sStackErrors,
    sEmbedHostDomains,
    sDescription,
    sRedirectURL,
  )
where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
import Network.AWS.AppStream.Types.StackError
import Network.AWS.AppStream.Types.StorageConnector
import Network.AWS.AppStream.Types.UserSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a stack.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
    userSettings :: Lude.Maybe (Lude.NonEmpty UserSetting),
    -- | The persistent application settings for users of the stack.
    applicationSettings :: Lude.Maybe ApplicationSettingsResponse,
    -- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Lude.Maybe Lude.Text,
    -- | The ARN of the stack.
    arn :: Lude.Maybe Lude.Text,
    -- | The time the stack was created.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The storage connectors to enable.
    storageConnectors :: Lude.Maybe [StorageConnector],
    -- | The list of virtual private cloud (VPC) interface endpoint objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
    accessEndpoints :: Lude.Maybe (Lude.NonEmpty AccessEndpoint),
    -- | The name of the stack.
    name :: Lude.Text,
    -- | The stack name to display.
    displayName :: Lude.Maybe Lude.Text,
    -- | The errors for the stack.
    stackErrors :: Lude.Maybe [StackError],
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
    embedHostDomains :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The description to display.
    description :: Lude.Maybe Lude.Text,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- * 'userSettings' - The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
-- * 'applicationSettings' - The persistent application settings for users of the stack.
-- * 'feedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
-- * 'arn' - The ARN of the stack.
-- * 'createdTime' - The time the stack was created.
-- * 'storageConnectors' - The storage connectors to enable.
-- * 'accessEndpoints' - The list of virtual private cloud (VPC) interface endpoint objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
-- * 'name' - The name of the stack.
-- * 'displayName' - The stack name to display.
-- * 'stackErrors' - The errors for the stack.
-- * 'embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
-- * 'description' - The description to display.
-- * 'redirectURL' - The URL that users are redirected to after their streaming session ends.
mkStack ::
  -- | 'name'
  Lude.Text ->
  Stack
mkStack pName_ =
  Stack'
    { userSettings = Lude.Nothing,
      applicationSettings = Lude.Nothing,
      feedbackURL = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      storageConnectors = Lude.Nothing,
      accessEndpoints = Lude.Nothing,
      name = pName_,
      displayName = Lude.Nothing,
      stackErrors = Lude.Nothing,
      embedHostDomains = Lude.Nothing,
      description = Lude.Nothing,
      redirectURL = Lude.Nothing
    }

-- | The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUserSettings :: Lens.Lens' Stack (Lude.Maybe (Lude.NonEmpty UserSetting))
sUserSettings = Lens.lens (userSettings :: Stack -> Lude.Maybe (Lude.NonEmpty UserSetting)) (\s a -> s {userSettings = a} :: Stack)
{-# DEPRECATED sUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The persistent application settings for users of the stack.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationSettings :: Lens.Lens' Stack (Lude.Maybe ApplicationSettingsResponse)
sApplicationSettings = Lens.lens (applicationSettings :: Stack -> Lude.Maybe ApplicationSettingsResponse) (\s a -> s {applicationSettings = a} :: Stack)
{-# DEPRECATED sApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFeedbackURL :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sFeedbackURL = Lens.lens (feedbackURL :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {feedbackURL = a} :: Stack)
{-# DEPRECATED sFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The ARN of the stack.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sARN = Lens.lens (arn :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Stack)
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the stack was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedTime :: Lens.Lens' Stack (Lude.Maybe Lude.Timestamp)
sCreatedTime = Lens.lens (createdTime :: Stack -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: Stack)
{-# DEPRECATED sCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStorageConnectors :: Lens.Lens' Stack (Lude.Maybe [StorageConnector])
sStorageConnectors = Lens.lens (storageConnectors :: Stack -> Lude.Maybe [StorageConnector]) (\s a -> s {storageConnectors = a} :: Stack)
{-# DEPRECATED sStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The list of virtual private cloud (VPC) interface endpoint objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccessEndpoints :: Lens.Lens' Stack (Lude.Maybe (Lude.NonEmpty AccessEndpoint))
sAccessEndpoints = Lens.lens (accessEndpoints :: Stack -> Lude.Maybe (Lude.NonEmpty AccessEndpoint)) (\s a -> s {accessEndpoints = a} :: Stack)
{-# DEPRECATED sAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Stack Lude.Text
sName = Lens.lens (name :: Stack -> Lude.Text) (\s a -> s {name = a} :: Stack)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDisplayName :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDisplayName = Lens.lens (displayName :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Stack)
{-# DEPRECATED sDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The errors for the stack.
--
-- /Note:/ Consider using 'stackErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackErrors :: Lens.Lens' Stack (Lude.Maybe [StackError])
sStackErrors = Lens.lens (stackErrors :: Stack -> Lude.Maybe [StackError]) (\s a -> s {stackErrors = a} :: Stack)
{-# DEPRECATED sStackErrors "Use generic-lens or generic-optics with 'stackErrors' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEmbedHostDomains :: Lens.Lens' Stack (Lude.Maybe (Lude.NonEmpty Lude.Text))
sEmbedHostDomains = Lens.lens (embedHostDomains :: Stack -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {embedHostDomains = a} :: Stack)
{-# DEPRECATED sEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDescription = Lens.lens (description :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Stack)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRedirectURL :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sRedirectURL = Lens.lens (redirectURL :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {redirectURL = a} :: Stack)
{-# DEPRECATED sRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

instance Lude.FromJSON Stack where
  parseJSON =
    Lude.withObject
      "Stack"
      ( \x ->
          Stack'
            Lude.<$> (x Lude..:? "UserSettings")
            Lude.<*> (x Lude..:? "ApplicationSettings")
            Lude.<*> (x Lude..:? "FeedbackURL")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "StorageConnectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccessEndpoints")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "StackErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EmbedHostDomains")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RedirectURL")
      )
