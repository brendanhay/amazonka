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
-- Module      : Amazonka.Nimble.Types.StreamConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingInstanceType
import qualified Amazonka.Prelude as Prelude

-- | A configuration for a streaming session.
--
-- /See:/ 'newStreamConfiguration' smart constructor.
data StreamConfiguration = StreamConfiguration'
  { -- | The length of time, in minutes, that a streaming session can run. After
    -- this point, Nimble Studio automatically terminates the session.
    maxSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The streaming images that users can select from when launching a
    -- streaming session with this launch profile.
    streamingImageIds :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 instance types that users can select from when launching a
    -- streaming session with this launch profile.
    ec2InstanceTypes :: Prelude.Maybe (Prelude.NonEmpty StreamingInstanceType),
    -- | Enable or disable the use of the system clipboard to copy and paste
    -- between the streaming session and streaming client.
    clipboardMode :: Prelude.Maybe StreamingClipboardMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSessionLengthInMinutes', 'streamConfiguration_maxSessionLengthInMinutes' - The length of time, in minutes, that a streaming session can run. After
-- this point, Nimble Studio automatically terminates the session.
--
-- 'streamingImageIds', 'streamConfiguration_streamingImageIds' - The streaming images that users can select from when launching a
-- streaming session with this launch profile.
--
-- 'ec2InstanceTypes', 'streamConfiguration_ec2InstanceTypes' - The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
--
-- 'clipboardMode', 'streamConfiguration_clipboardMode' - Enable or disable the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
newStreamConfiguration ::
  StreamConfiguration
newStreamConfiguration =
  StreamConfiguration'
    { maxSessionLengthInMinutes =
        Prelude.Nothing,
      streamingImageIds = Prelude.Nothing,
      ec2InstanceTypes = Prelude.Nothing,
      clipboardMode = Prelude.Nothing
    }

-- | The length of time, in minutes, that a streaming session can run. After
-- this point, Nimble Studio automatically terminates the session.
streamConfiguration_maxSessionLengthInMinutes :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Natural)
streamConfiguration_maxSessionLengthInMinutes = Lens.lens (\StreamConfiguration' {maxSessionLengthInMinutes} -> maxSessionLengthInMinutes) (\s@StreamConfiguration' {} a -> s {maxSessionLengthInMinutes = a} :: StreamConfiguration)

-- | The streaming images that users can select from when launching a
-- streaming session with this launch profile.
streamConfiguration_streamingImageIds :: Lens.Lens' StreamConfiguration (Prelude.Maybe [Prelude.Text])
streamConfiguration_streamingImageIds = Lens.lens (\StreamConfiguration' {streamingImageIds} -> streamingImageIds) (\s@StreamConfiguration' {} a -> s {streamingImageIds = a} :: StreamConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
streamConfiguration_ec2InstanceTypes :: Lens.Lens' StreamConfiguration (Prelude.Maybe (Prelude.NonEmpty StreamingInstanceType))
streamConfiguration_ec2InstanceTypes = Lens.lens (\StreamConfiguration' {ec2InstanceTypes} -> ec2InstanceTypes) (\s@StreamConfiguration' {} a -> s {ec2InstanceTypes = a} :: StreamConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Enable or disable the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
streamConfiguration_clipboardMode :: Lens.Lens' StreamConfiguration (Prelude.Maybe StreamingClipboardMode)
streamConfiguration_clipboardMode = Lens.lens (\StreamConfiguration' {clipboardMode} -> clipboardMode) (\s@StreamConfiguration' {} a -> s {clipboardMode = a} :: StreamConfiguration)

instance Core.FromJSON StreamConfiguration where
  parseJSON =
    Core.withObject
      "StreamConfiguration"
      ( \x ->
          StreamConfiguration'
            Prelude.<$> (x Core..:? "maxSessionLengthInMinutes")
            Prelude.<*> ( x Core..:? "streamingImageIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ec2InstanceTypes")
            Prelude.<*> (x Core..:? "clipboardMode")
      )

instance Prelude.Hashable StreamConfiguration where
  hashWithSalt _salt StreamConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` maxSessionLengthInMinutes
      `Prelude.hashWithSalt` streamingImageIds
      `Prelude.hashWithSalt` ec2InstanceTypes
      `Prelude.hashWithSalt` clipboardMode

instance Prelude.NFData StreamConfiguration where
  rnf StreamConfiguration' {..} =
    Prelude.rnf maxSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf streamingImageIds
      `Prelude.seq` Prelude.rnf ec2InstanceTypes
      `Prelude.seq` Prelude.rnf clipboardMode
