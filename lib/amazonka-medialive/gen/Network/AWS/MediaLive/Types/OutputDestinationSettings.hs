{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputDestinationSettings
  ( OutputDestinationSettings (..),

    -- * Smart constructor
    mkOutputDestinationSettings,

    -- * Lenses
    odsURL,
    odsUsername,
    odsPasswordParam,
    odsStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'mkOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { url ::
      Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    passwordParam :: Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDestinationSettings' with the minimum fields required to make a request.
--
-- * 'passwordParam' - key used to extract the password from EC2 Parameter store
-- * 'streamName' - Stream name for RTMP destinations (URLs of type rtmp://)
-- * 'url' - A URL specifying a destination
-- * 'username' - username for destination
mkOutputDestinationSettings ::
  OutputDestinationSettings
mkOutputDestinationSettings =
  OutputDestinationSettings'
    { url = Lude.Nothing,
      username = Lude.Nothing,
      passwordParam = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | A URL specifying a destination
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsURL :: Lens.Lens' OutputDestinationSettings (Lude.Maybe Lude.Text)
odsURL = Lens.lens (url :: OutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: OutputDestinationSettings)
{-# DEPRECATED odsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | username for destination
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsUsername :: Lens.Lens' OutputDestinationSettings (Lude.Maybe Lude.Text)
odsUsername = Lens.lens (username :: OutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: OutputDestinationSettings)
{-# DEPRECATED odsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | key used to extract the password from EC2 Parameter store
--
-- /Note:/ Consider using 'passwordParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsPasswordParam :: Lens.Lens' OutputDestinationSettings (Lude.Maybe Lude.Text)
odsPasswordParam = Lens.lens (passwordParam :: OutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {passwordParam = a} :: OutputDestinationSettings)
{-# DEPRECATED odsPasswordParam "Use generic-lens or generic-optics with 'passwordParam' instead." #-}

-- | Stream name for RTMP destinations (URLs of type rtmp://)
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odsStreamName :: Lens.Lens' OutputDestinationSettings (Lude.Maybe Lude.Text)
odsStreamName = Lens.lens (streamName :: OutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: OutputDestinationSettings)
{-# DEPRECATED odsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON OutputDestinationSettings where
  parseJSON =
    Lude.withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            Lude.<$> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "passwordParam")
            Lude.<*> (x Lude..:? "streamName")
      )

instance Lude.ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("url" Lude..=) Lude.<$> url,
            ("username" Lude..=) Lude.<$> username,
            ("passwordParam" Lude..=) Lude.<$> passwordParam,
            ("streamName" Lude..=) Lude.<$> streamName
          ]
      )
