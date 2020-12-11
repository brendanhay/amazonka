{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csUserSettings,
    csApplicationSettings,
    csFeedbackURL,
    csStorageConnectors,
    csAccessEndpoints,
    csDisplayName,
    csEmbedHostDomains,
    csDescription,
    csTags,
    csRedirectURL,
    csName,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    csrsStack,
    csrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { userSettings ::
      Lude.Maybe (Lude.NonEmpty UserSetting),
    applicationSettings :: Lude.Maybe ApplicationSettings,
    feedbackURL :: Lude.Maybe Lude.Text,
    storageConnectors :: Lude.Maybe [StorageConnector],
    accessEndpoints :: Lude.Maybe (Lude.NonEmpty AccessEndpoint),
    displayName :: Lude.Maybe Lude.Text,
    embedHostDomains :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'CreateStack' with the minimum fields required to make a request.
--
-- * 'accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
-- * 'applicationSettings' - The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
-- * 'description' - The description to display.
-- * 'displayName' - The stack name to display.
-- * 'embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
-- * 'feedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
-- * 'name' - The name of the stack.
-- * 'redirectURL' - The URL that users are redirected to after their streaming session ends.
-- * 'storageConnectors' - The storage connectors to enable.
-- * 'tags' - The tags to associate with the stack. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'userSettings' - The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
mkCreateStack ::
  -- | 'name'
  Lude.Text ->
  CreateStack
mkCreateStack pName_ =
  CreateStack'
    { userSettings = Lude.Nothing,
      applicationSettings = Lude.Nothing,
      feedbackURL = Lude.Nothing,
      storageConnectors = Lude.Nothing,
      accessEndpoints = Lude.Nothing,
      displayName = Lude.Nothing,
      embedHostDomains = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      redirectURL = Lude.Nothing,
      name = pName_
    }

-- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUserSettings :: Lens.Lens' CreateStack (Lude.Maybe (Lude.NonEmpty UserSetting))
csUserSettings = Lens.lens (userSettings :: CreateStack -> Lude.Maybe (Lude.NonEmpty UserSetting)) (\s a -> s {userSettings = a} :: CreateStack)
{-# DEPRECATED csUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
--
-- /Note:/ Consider using 'applicationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csApplicationSettings :: Lens.Lens' CreateStack (Lude.Maybe ApplicationSettings)
csApplicationSettings = Lens.lens (applicationSettings :: CreateStack -> Lude.Maybe ApplicationSettings) (\s a -> s {applicationSettings = a} :: CreateStack)
{-# DEPRECATED csApplicationSettings "Use generic-lens or generic-optics with 'applicationSettings' instead." #-}

-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- /Note:/ Consider using 'feedbackURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csFeedbackURL :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csFeedbackURL = Lens.lens (feedbackURL :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {feedbackURL = a} :: CreateStack)
{-# DEPRECATED csFeedbackURL "Use generic-lens or generic-optics with 'feedbackURL' instead." #-}

-- | The storage connectors to enable.
--
-- /Note:/ Consider using 'storageConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStorageConnectors :: Lens.Lens' CreateStack (Lude.Maybe [StorageConnector])
csStorageConnectors = Lens.lens (storageConnectors :: CreateStack -> Lude.Maybe [StorageConnector]) (\s a -> s {storageConnectors = a} :: CreateStack)
{-# DEPRECATED csStorageConnectors "Use generic-lens or generic-optics with 'storageConnectors' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAccessEndpoints :: Lens.Lens' CreateStack (Lude.Maybe (Lude.NonEmpty AccessEndpoint))
csAccessEndpoints = Lens.lens (accessEndpoints :: CreateStack -> Lude.Maybe (Lude.NonEmpty AccessEndpoint)) (\s a -> s {accessEndpoints = a} :: CreateStack)
{-# DEPRECATED csAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The stack name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisplayName :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDisplayName = Lens.lens (displayName :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateStack)
{-# DEPRECATED csDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- /Note:/ Consider using 'embedHostDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEmbedHostDomains :: Lens.Lens' CreateStack (Lude.Maybe (Lude.NonEmpty Lude.Text))
csEmbedHostDomains = Lens.lens (embedHostDomains :: CreateStack -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {embedHostDomains = a} :: CreateStack)
{-# DEPRECATED csEmbedHostDomains "Use generic-lens or generic-optics with 'embedHostDomains' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStack)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to associate with the stack. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStack (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csTags = Lens.lens (tags :: CreateStack -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateStack)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The URL that users are redirected to after their streaming session ends.
--
-- /Note:/ Consider using 'redirectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRedirectURL :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csRedirectURL = Lens.lens (redirectURL :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {redirectURL = a} :: CreateStack)
{-# DEPRECATED csRedirectURL "Use generic-lens or generic-optics with 'redirectURL' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateStack Lude.Text
csName = Lens.lens (name :: CreateStack -> Lude.Text) (\s a -> s {name = a} :: CreateStack)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Lude.<$> (x Lude..?> "Stack") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CreateStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserSettings" Lude..=) Lude.<$> userSettings,
            ("ApplicationSettings" Lude..=) Lude.<$> applicationSettings,
            ("FeedbackURL" Lude..=) Lude.<$> feedbackURL,
            ("StorageConnectors" Lude..=) Lude.<$> storageConnectors,
            ("AccessEndpoints" Lude..=) Lude.<$> accessEndpoints,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("EmbedHostDomains" Lude..=) Lude.<$> embedHostDomains,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            ("RedirectURL" Lude..=) Lude.<$> redirectURL,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
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

-- | Creates a value of 'CreateStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stack' - Information about the stack.
mkCreateStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStackResponse
mkCreateStackResponse pResponseStatus_ =
  CreateStackResponse'
    { stack = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the stack.
--
-- /Note:/ Consider using 'stack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStack :: Lens.Lens' CreateStackResponse (Lude.Maybe Stack)
csrsStack = Lens.lens (stack :: CreateStackResponse -> Lude.Maybe Stack) (\s a -> s {stack = a} :: CreateStackResponse)
{-# DEPRECATED csrsStack "Use generic-lens or generic-optics with 'stack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateStackResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStackResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
