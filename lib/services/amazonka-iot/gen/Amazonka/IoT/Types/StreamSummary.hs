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
-- Module      : Amazonka.IoT.Types.StreamSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of a stream.
--
-- /See:/ 'newStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | A description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stream version.
    streamVersion :: Prelude.Maybe Prelude.Natural,
    -- | The stream ARN.
    streamArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'streamSummary_streamId' - The stream ID.
--
-- 'description', 'streamSummary_description' - A description of the stream.
--
-- 'streamVersion', 'streamSummary_streamVersion' - The stream version.
--
-- 'streamArn', 'streamSummary_streamArn' - The stream ARN.
newStreamSummary ::
  StreamSummary
newStreamSummary =
  StreamSummary'
    { streamId = Prelude.Nothing,
      description = Prelude.Nothing,
      streamVersion = Prelude.Nothing,
      streamArn = Prelude.Nothing
    }

-- | The stream ID.
streamSummary_streamId :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_streamId = Lens.lens (\StreamSummary' {streamId} -> streamId) (\s@StreamSummary' {} a -> s {streamId = a} :: StreamSummary)

-- | A description of the stream.
streamSummary_description :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_description = Lens.lens (\StreamSummary' {description} -> description) (\s@StreamSummary' {} a -> s {description = a} :: StreamSummary)

-- | The stream version.
streamSummary_streamVersion :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Natural)
streamSummary_streamVersion = Lens.lens (\StreamSummary' {streamVersion} -> streamVersion) (\s@StreamSummary' {} a -> s {streamVersion = a} :: StreamSummary)

-- | The stream ARN.
streamSummary_streamArn :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_streamArn = Lens.lens (\StreamSummary' {streamArn} -> streamArn) (\s@StreamSummary' {} a -> s {streamArn = a} :: StreamSummary)

instance Core.FromJSON StreamSummary where
  parseJSON =
    Core.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Prelude.<$> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "streamVersion")
            Prelude.<*> (x Core..:? "streamArn")
      )

instance Prelude.Hashable StreamSummary where
  hashWithSalt _salt StreamSummary' {..} =
    _salt `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` streamVersion
      `Prelude.hashWithSalt` streamArn

instance Prelude.NFData StreamSummary where
  rnf StreamSummary' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamVersion
      `Prelude.seq` Prelude.rnf streamArn
