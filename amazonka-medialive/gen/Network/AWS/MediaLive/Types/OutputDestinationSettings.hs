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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'newOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Core.Maybe Core.Text,
    -- | username for destination
    username :: Core.Maybe Core.Text,
    -- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
    streamName :: Core.Maybe Core.Text,
    -- | A URL specifying a destination
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      username = Core.Nothing,
      streamName = Core.Nothing,
      url = Core.Nothing
    }

-- | key used to extract the password from EC2 Parameter store
outputDestinationSettings_passwordParam :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
outputDestinationSettings_passwordParam = Lens.lens (\OutputDestinationSettings' {passwordParam} -> passwordParam) (\s@OutputDestinationSettings' {} a -> s {passwordParam = a} :: OutputDestinationSettings)

-- | username for destination
outputDestinationSettings_username :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
outputDestinationSettings_username = Lens.lens (\OutputDestinationSettings' {username} -> username) (\s@OutputDestinationSettings' {} a -> s {username = a} :: OutputDestinationSettings)

-- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
outputDestinationSettings_streamName :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
outputDestinationSettings_streamName = Lens.lens (\OutputDestinationSettings' {streamName} -> streamName) (\s@OutputDestinationSettings' {} a -> s {streamName = a} :: OutputDestinationSettings)

-- | A URL specifying a destination
outputDestinationSettings_url :: Lens.Lens' OutputDestinationSettings (Core.Maybe Core.Text)
outputDestinationSettings_url = Lens.lens (\OutputDestinationSettings' {url} -> url) (\s@OutputDestinationSettings' {} a -> s {url = a} :: OutputDestinationSettings)

instance Core.FromJSON OutputDestinationSettings where
  parseJSON =
    Core.withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            Core.<$> (x Core..:? "passwordParam")
            Core.<*> (x Core..:? "username")
            Core.<*> (x Core..:? "streamName")
            Core.<*> (x Core..:? "url")
      )

instance Core.Hashable OutputDestinationSettings

instance Core.NFData OutputDestinationSettings

instance Core.ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("passwordParam" Core..=) Core.<$> passwordParam,
            ("username" Core..=) Core.<$> username,
            ("streamName" Core..=) Core.<$> streamName,
            ("url" Core..=) Core.<$> url
          ]
      )
