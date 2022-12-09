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
-- Module      : Amazonka.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'newOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
    streamName :: Prelude.Maybe Prelude.Text,
    -- | A URL specifying a destination
    url :: Prelude.Maybe Prelude.Text,
    -- | username for destination
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'streamName', 'outputDestinationSettings_streamName' - Stream name for RTMP destinations (URLs of type rtmp:\/\/)
--
-- 'url', 'outputDestinationSettings_url' - A URL specifying a destination
--
-- 'username', 'outputDestinationSettings_username' - username for destination
newOutputDestinationSettings ::
  OutputDestinationSettings
newOutputDestinationSettings =
  OutputDestinationSettings'
    { passwordParam =
        Prelude.Nothing,
      streamName = Prelude.Nothing,
      url = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | key used to extract the password from EC2 Parameter store
outputDestinationSettings_passwordParam :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_passwordParam = Lens.lens (\OutputDestinationSettings' {passwordParam} -> passwordParam) (\s@OutputDestinationSettings' {} a -> s {passwordParam = a} :: OutputDestinationSettings)

-- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
outputDestinationSettings_streamName :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_streamName = Lens.lens (\OutputDestinationSettings' {streamName} -> streamName) (\s@OutputDestinationSettings' {} a -> s {streamName = a} :: OutputDestinationSettings)

-- | A URL specifying a destination
outputDestinationSettings_url :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_url = Lens.lens (\OutputDestinationSettings' {url} -> url) (\s@OutputDestinationSettings' {} a -> s {url = a} :: OutputDestinationSettings)

-- | username for destination
outputDestinationSettings_username :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_username = Lens.lens (\OutputDestinationSettings' {username} -> username) (\s@OutputDestinationSettings' {} a -> s {username = a} :: OutputDestinationSettings)

instance Data.FromJSON OutputDestinationSettings where
  parseJSON =
    Data.withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            Prelude.<$> (x Data..:? "passwordParam")
            Prelude.<*> (x Data..:? "streamName")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "username")
      )

instance Prelude.Hashable OutputDestinationSettings where
  hashWithSalt _salt OutputDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` passwordParam
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` username

instance Prelude.NFData OutputDestinationSettings where
  rnf OutputDestinationSettings' {..} =
    Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("passwordParam" Data..=) Prelude.<$> passwordParam,
            ("streamName" Data..=) Prelude.<$> streamName,
            ("url" Data..=) Prelude.<$> url,
            ("username" Data..=) Prelude.<$> username
          ]
      )
