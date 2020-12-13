{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMMessage
  ( GCMMessage (..),

    -- * Smart constructor
    mkGCMMessage,

    -- * Lenses
    gmSubstitutions,
    gmSilentPush,
    gmImageIconURL,
    gmPriority,
    gmRawContent,
    gmData,
    gmRestrictedPackageName,
    gmSmallImageIconURL,
    gmBody,
    gmTimeToLive,
    gmURL,
    gmSound,
    gmAction,
    gmCollapseKey,
    gmImageURL,
    gmTitle,
    gmIconReference,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the GCM channel. The GCM channel enables Amazon Pinpoint to send messages to the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMMessage' smart constructor.
data GCMMessage = GCMMessage'
  { -- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
    substitutions :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
    silentPush :: Lude.Maybe Lude.Bool,
    -- | The URL of the large icon image to display in the content view of the push notification.
    imageIconURL :: Lude.Maybe Lude.Text,
    -- | para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required.
    --
    -- /listitem>
    --     * high - The notification is sent immediately and might wake a sleeping device.
    --
    -- /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM.
    -- The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
    priority :: Lude.Maybe Lude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
    rawContent :: Lude.Maybe Lude.Text,
    -- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The package name of the application where registration tokens must match in order for the recipient to receive the message.
    restrictedPackageName :: Lude.Maybe Lude.Text,
    -- | The URL of the small icon image to display in the status bar and the content view of the push notification.
    smallImageIconURL :: Lude.Maybe Lude.Text,
    -- | The body of the notification message.
    body :: Lude.Maybe Lude.Text,
    -- | The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days).
    --
    -- Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
    timeToLive :: Lude.Maybe Lude.Int,
    -- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
    url :: Lude.Maybe Lude.Text,
    -- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
    sound :: Lude.Maybe Lude.Text,
    -- | The action to occur if the recipient taps the push notification. Valid values are:
    --
    --
    --     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
    --
    --
    --     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
    --
    --
    --     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
    action :: Lude.Maybe Action,
    -- | An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active.
    --
    -- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
    collapseKey :: Lude.Maybe Lude.Text,
    -- | The URL of an image to display in the push notification.
    imageURL :: Lude.Maybe Lude.Text,
    -- | The title to display above the notification message on the recipient's device.
    title :: Lude.Maybe Lude.Text,
    -- | The icon image name of the asset saved in your app.
    iconReference :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GCMMessage' with the minimum fields required to make a request.
--
-- * 'substitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
-- * 'silentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
-- * 'imageIconURL' - The URL of the large icon image to display in the content view of the push notification.
-- * 'priority' - para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required.
--
-- /listitem>
--     * high - The notification is sent immediately and might wake a sleeping device.
--
-- /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM.
-- The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
-- * 'data'' - The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
-- * 'restrictedPackageName' - The package name of the application where registration tokens must match in order for the recipient to receive the message.
-- * 'smallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of the push notification.
-- * 'body' - The body of the notification message.
-- * 'timeToLive' - The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
-- * 'url' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
-- * 'sound' - The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
-- * 'action' - The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
-- * 'collapseKey' - An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
-- * 'imageURL' - The URL of an image to display in the push notification.
-- * 'title' - The title to display above the notification message on the recipient's device.
-- * 'iconReference' - The icon image name of the asset saved in your app.
mkGCMMessage ::
  GCMMessage
mkGCMMessage =
  GCMMessage'
    { substitutions = Lude.Nothing,
      silentPush = Lude.Nothing,
      imageIconURL = Lude.Nothing,
      priority = Lude.Nothing,
      rawContent = Lude.Nothing,
      data' = Lude.Nothing,
      restrictedPackageName = Lude.Nothing,
      smallImageIconURL = Lude.Nothing,
      body = Lude.Nothing,
      timeToLive = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      collapseKey = Lude.Nothing,
      imageURL = Lude.Nothing,
      title = Lude.Nothing,
      iconReference = Lude.Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSubstitutions :: Lens.Lens' GCMMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
gmSubstitutions = Lens.lens (substitutions :: GCMMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: GCMMessage)
{-# DEPRECATED gmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSilentPush :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Bool)
gmSilentPush = Lens.lens (silentPush :: GCMMessage -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: GCMMessage)
{-# DEPRECATED gmSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmImageIconURL :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmImageIconURL = Lens.lens (imageIconURL :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageIconURL = a} :: GCMMessage)
{-# DEPRECATED gmImageIconURL "Use generic-lens or generic-optics with 'imageIconURL' instead." #-}

-- | para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required.
--
-- /listitem>
--     * high - The notification is sent immediately and might wake a sleeping device.
--
-- /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM.
-- The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmPriority :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmPriority = Lens.lens (priority :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {priority = a} :: GCMMessage)
{-# DEPRECATED gmPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmRawContent :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmRawContent = Lens.lens (rawContent :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: GCMMessage)
{-# DEPRECATED gmRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmData :: Lens.Lens' GCMMessage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gmData = Lens.lens (data' :: GCMMessage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {data' = a} :: GCMMessage)
{-# DEPRECATED gmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The package name of the application where registration tokens must match in order for the recipient to receive the message.
--
-- /Note:/ Consider using 'restrictedPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmRestrictedPackageName :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmRestrictedPackageName = Lens.lens (restrictedPackageName :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {restrictedPackageName = a} :: GCMMessage)
{-# DEPRECATED gmRestrictedPackageName "Use generic-lens or generic-optics with 'restrictedPackageName' instead." #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSmallImageIconURL :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmSmallImageIconURL = Lens.lens (smallImageIconURL :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {smallImageIconURL = a} :: GCMMessage)
{-# DEPRECATED gmSmallImageIconURL "Use generic-lens or generic-optics with 'smallImageIconURL' instead." #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmBody :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmBody = Lens.lens (body :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: GCMMessage)
{-# DEPRECATED gmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmTimeToLive :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Int)
gmTimeToLive = Lens.lens (timeToLive :: GCMMessage -> Lude.Maybe Lude.Int) (\s a -> s {timeToLive = a} :: GCMMessage)
{-# DEPRECATED gmTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmURL :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmURL = Lens.lens (url :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: GCMMessage)
{-# DEPRECATED gmURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSound :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmSound = Lens.lens (sound :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: GCMMessage)
{-# DEPRECATED gmSound "Use generic-lens or generic-optics with 'sound' instead." #-}

-- | The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmAction :: Lens.Lens' GCMMessage (Lude.Maybe Action)
gmAction = Lens.lens (action :: GCMMessage -> Lude.Maybe Action) (\s a -> s {action = a} :: GCMMessage)
{-# DEPRECATED gmAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
--
-- /Note:/ Consider using 'collapseKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmCollapseKey :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmCollapseKey = Lens.lens (collapseKey :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {collapseKey = a} :: GCMMessage)
{-# DEPRECATED gmCollapseKey "Use generic-lens or generic-optics with 'collapseKey' instead." #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmImageURL :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmImageURL = Lens.lens (imageURL :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: GCMMessage)
{-# DEPRECATED gmImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmTitle :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmTitle = Lens.lens (title :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: GCMMessage)
{-# DEPRECATED gmTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmIconReference :: Lens.Lens' GCMMessage (Lude.Maybe Lude.Text)
gmIconReference = Lens.lens (iconReference :: GCMMessage -> Lude.Maybe Lude.Text) (\s a -> s {iconReference = a} :: GCMMessage)
{-# DEPRECATED gmIconReference "Use generic-lens or generic-optics with 'iconReference' instead." #-}

instance Lude.ToJSON GCMMessage where
  toJSON GCMMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("ImageIconUrl" Lude..=) Lude.<$> imageIconURL,
            ("Priority" Lude..=) Lude.<$> priority,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Data" Lude..=) Lude.<$> data',
            ("RestrictedPackageName" Lude..=) Lude.<$> restrictedPackageName,
            ("SmallImageIconUrl" Lude..=) Lude.<$> smallImageIconURL,
            ("Body" Lude..=) Lude.<$> body,
            ("TimeToLive" Lude..=) Lude.<$> timeToLive,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("CollapseKey" Lude..=) Lude.<$> collapseKey,
            ("ImageUrl" Lude..=) Lude.<$> imageURL,
            ("Title" Lude..=) Lude.<$> title,
            ("IconReference" Lude..=) Lude.<$> iconReference
          ]
      )
