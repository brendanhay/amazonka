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
-- Module      : Amazonka.Nimble.Types.StreamConfigurationCreate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfigurationCreate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingInstanceType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newStreamConfigurationCreate' smart constructor.
data StreamConfigurationCreate = StreamConfigurationCreate'
  { -- | The length of time, in minutes, that a streaming session can run. After
    -- this point, Nimble Studio automatically terminates the session.
    maxSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Enable or disable the use of the system clipboard to copy and paste
    -- between the streaming session and streaming client.
    clipboardMode :: StreamingClipboardMode,
    -- | The streaming images that users can select from when launching a
    -- streaming session with this launch profile.
    streamingImageIds :: [Prelude.Text],
    -- | The EC2 instance types that users can select from when launching a
    -- streaming session with this launch profile.
    ec2InstanceTypes :: Prelude.NonEmpty StreamingInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamConfigurationCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSessionLengthInMinutes', 'streamConfigurationCreate_maxSessionLengthInMinutes' - The length of time, in minutes, that a streaming session can run. After
-- this point, Nimble Studio automatically terminates the session.
--
-- 'clipboardMode', 'streamConfigurationCreate_clipboardMode' - Enable or disable the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
--
-- 'streamingImageIds', 'streamConfigurationCreate_streamingImageIds' - The streaming images that users can select from when launching a
-- streaming session with this launch profile.
--
-- 'ec2InstanceTypes', 'streamConfigurationCreate_ec2InstanceTypes' - The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
newStreamConfigurationCreate ::
  -- | 'clipboardMode'
  StreamingClipboardMode ->
  -- | 'ec2InstanceTypes'
  Prelude.NonEmpty StreamingInstanceType ->
  StreamConfigurationCreate
newStreamConfigurationCreate
  pClipboardMode_
  pEc2InstanceTypes_ =
    StreamConfigurationCreate'
      { maxSessionLengthInMinutes =
          Prelude.Nothing,
        clipboardMode = pClipboardMode_,
        streamingImageIds = Prelude.mempty,
        ec2InstanceTypes =
          Lens.coerced Lens.# pEc2InstanceTypes_
      }

-- | The length of time, in minutes, that a streaming session can run. After
-- this point, Nimble Studio automatically terminates the session.
streamConfigurationCreate_maxSessionLengthInMinutes :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe Prelude.Natural)
streamConfigurationCreate_maxSessionLengthInMinutes = Lens.lens (\StreamConfigurationCreate' {maxSessionLengthInMinutes} -> maxSessionLengthInMinutes) (\s@StreamConfigurationCreate' {} a -> s {maxSessionLengthInMinutes = a} :: StreamConfigurationCreate)

-- | Enable or disable the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
streamConfigurationCreate_clipboardMode :: Lens.Lens' StreamConfigurationCreate StreamingClipboardMode
streamConfigurationCreate_clipboardMode = Lens.lens (\StreamConfigurationCreate' {clipboardMode} -> clipboardMode) (\s@StreamConfigurationCreate' {} a -> s {clipboardMode = a} :: StreamConfigurationCreate)

-- | The streaming images that users can select from when launching a
-- streaming session with this launch profile.
streamConfigurationCreate_streamingImageIds :: Lens.Lens' StreamConfigurationCreate [Prelude.Text]
streamConfigurationCreate_streamingImageIds = Lens.lens (\StreamConfigurationCreate' {streamingImageIds} -> streamingImageIds) (\s@StreamConfigurationCreate' {} a -> s {streamingImageIds = a} :: StreamConfigurationCreate) Prelude.. Lens.coerced

-- | The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
streamConfigurationCreate_ec2InstanceTypes :: Lens.Lens' StreamConfigurationCreate (Prelude.NonEmpty StreamingInstanceType)
streamConfigurationCreate_ec2InstanceTypes = Lens.lens (\StreamConfigurationCreate' {ec2InstanceTypes} -> ec2InstanceTypes) (\s@StreamConfigurationCreate' {} a -> s {ec2InstanceTypes = a} :: StreamConfigurationCreate) Prelude.. Lens.coerced

instance Prelude.Hashable StreamConfigurationCreate where
  hashWithSalt _salt StreamConfigurationCreate' {..} =
    _salt
      `Prelude.hashWithSalt` maxSessionLengthInMinutes
      `Prelude.hashWithSalt` clipboardMode
      `Prelude.hashWithSalt` streamingImageIds
      `Prelude.hashWithSalt` ec2InstanceTypes

instance Prelude.NFData StreamConfigurationCreate where
  rnf StreamConfigurationCreate' {..} =
    Prelude.rnf maxSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf clipboardMode
      `Prelude.seq` Prelude.rnf streamingImageIds
      `Prelude.seq` Prelude.rnf ec2InstanceTypes

instance Core.ToJSON StreamConfigurationCreate where
  toJSON StreamConfigurationCreate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxSessionLengthInMinutes" Core..=)
              Prelude.<$> maxSessionLengthInMinutes,
            Prelude.Just ("clipboardMode" Core..= clipboardMode),
            Prelude.Just
              ("streamingImageIds" Core..= streamingImageIds),
            Prelude.Just
              ("ec2InstanceTypes" Core..= ec2InstanceTypes)
          ]
      )
