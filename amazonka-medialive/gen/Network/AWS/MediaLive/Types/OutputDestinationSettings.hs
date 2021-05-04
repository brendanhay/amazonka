{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'newOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | username for destination
    username :: Prelude.Maybe Prelude.Text,
    -- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
    streamName :: Prelude.Maybe Prelude.Text,
    -- | A URL specifying a destination
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordParam', 'outputDestinationSettings_passwordParam' - key used to extract the password from EC2 Parameter store
--
-- 'username', 'outputDestinationSettings_username' - username for destination
--
-- 'streamName', 'outputDestinationSettings_streamName' - Stream name for RTMP destinations (URLs of type rtmp:\/\/)
--
-- 'url', 'outputDestinationSettings_url' - A URL specifying a destination
newOutputDestinationSettings ::
  OutputDestinationSettings
newOutputDestinationSettings =
  OutputDestinationSettings'
    { passwordParam =
        Prelude.Nothing,
      username = Prelude.Nothing,
      streamName = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | key used to extract the password from EC2 Parameter store
outputDestinationSettings_passwordParam :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_passwordParam = Lens.lens (\OutputDestinationSettings' {passwordParam} -> passwordParam) (\s@OutputDestinationSettings' {} a -> s {passwordParam = a} :: OutputDestinationSettings)

-- | username for destination
outputDestinationSettings_username :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_username = Lens.lens (\OutputDestinationSettings' {username} -> username) (\s@OutputDestinationSettings' {} a -> s {username = a} :: OutputDestinationSettings)

-- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
outputDestinationSettings_streamName :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_streamName = Lens.lens (\OutputDestinationSettings' {streamName} -> streamName) (\s@OutputDestinationSettings' {} a -> s {streamName = a} :: OutputDestinationSettings)

-- | A URL specifying a destination
outputDestinationSettings_url :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_url = Lens.lens (\OutputDestinationSettings' {url} -> url) (\s@OutputDestinationSettings' {} a -> s {url = a} :: OutputDestinationSettings)

instance Prelude.FromJSON OutputDestinationSettings where
  parseJSON =
    Prelude.withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            Prelude.<$> (x Prelude..:? "passwordParam")
            Prelude.<*> (x Prelude..:? "username")
            Prelude.<*> (x Prelude..:? "streamName")
            Prelude.<*> (x Prelude..:? "url")
      )

instance Prelude.Hashable OutputDestinationSettings

instance Prelude.NFData OutputDestinationSettings

instance Prelude.ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("passwordParam" Prelude..=)
              Prelude.<$> passwordParam,
            ("username" Prelude..=) Prelude.<$> username,
            ("streamName" Prelude..=) Prelude.<$> streamName,
            ("url" Prelude..=) Prelude.<$> url
          ]
      )
