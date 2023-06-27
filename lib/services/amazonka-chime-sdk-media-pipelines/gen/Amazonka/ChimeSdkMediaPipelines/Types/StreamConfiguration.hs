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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.StreamConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.StreamConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.StreamChannelDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for a stream.
--
-- /See:/ 'newStreamConfiguration' smart constructor.
data StreamConfiguration = StreamConfiguration'
  { -- | The unique identifier of the fragment to begin processing.
    fragmentNumber :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stream.
    streamArn :: Prelude.Text,
    -- | The streaming channel definition in the stream configuration.
    streamChannelDefinition :: StreamChannelDefinition
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
-- 'fragmentNumber', 'streamConfiguration_fragmentNumber' - The unique identifier of the fragment to begin processing.
--
-- 'streamArn', 'streamConfiguration_streamArn' - The ARN of the stream.
--
-- 'streamChannelDefinition', 'streamConfiguration_streamChannelDefinition' - The streaming channel definition in the stream configuration.
newStreamConfiguration ::
  -- | 'streamArn'
  Prelude.Text ->
  -- | 'streamChannelDefinition'
  StreamChannelDefinition ->
  StreamConfiguration
newStreamConfiguration
  pStreamArn_
  pStreamChannelDefinition_ =
    StreamConfiguration'
      { fragmentNumber =
          Prelude.Nothing,
        streamArn = pStreamArn_,
        streamChannelDefinition = pStreamChannelDefinition_
      }

-- | The unique identifier of the fragment to begin processing.
streamConfiguration_fragmentNumber :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Text)
streamConfiguration_fragmentNumber = Lens.lens (\StreamConfiguration' {fragmentNumber} -> fragmentNumber) (\s@StreamConfiguration' {} a -> s {fragmentNumber = a} :: StreamConfiguration)

-- | The ARN of the stream.
streamConfiguration_streamArn :: Lens.Lens' StreamConfiguration Prelude.Text
streamConfiguration_streamArn = Lens.lens (\StreamConfiguration' {streamArn} -> streamArn) (\s@StreamConfiguration' {} a -> s {streamArn = a} :: StreamConfiguration)

-- | The streaming channel definition in the stream configuration.
streamConfiguration_streamChannelDefinition :: Lens.Lens' StreamConfiguration StreamChannelDefinition
streamConfiguration_streamChannelDefinition = Lens.lens (\StreamConfiguration' {streamChannelDefinition} -> streamChannelDefinition) (\s@StreamConfiguration' {} a -> s {streamChannelDefinition = a} :: StreamConfiguration)

instance Data.FromJSON StreamConfiguration where
  parseJSON =
    Data.withObject
      "StreamConfiguration"
      ( \x ->
          StreamConfiguration'
            Prelude.<$> (x Data..:? "FragmentNumber")
            Prelude.<*> (x Data..: "StreamArn")
            Prelude.<*> (x Data..: "StreamChannelDefinition")
      )

instance Prelude.Hashable StreamConfiguration where
  hashWithSalt _salt StreamConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fragmentNumber
      `Prelude.hashWithSalt` streamArn
      `Prelude.hashWithSalt` streamChannelDefinition

instance Prelude.NFData StreamConfiguration where
  rnf StreamConfiguration' {..} =
    Prelude.rnf fragmentNumber
      `Prelude.seq` Prelude.rnf streamArn
      `Prelude.seq` Prelude.rnf streamChannelDefinition

instance Data.ToJSON StreamConfiguration where
  toJSON StreamConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FragmentNumber" Data..=)
              Prelude.<$> fragmentNumber,
            Prelude.Just ("StreamArn" Data..= streamArn),
            Prelude.Just
              ( "StreamChannelDefinition"
                  Data..= streamChannelDefinition
              )
          ]
      )
