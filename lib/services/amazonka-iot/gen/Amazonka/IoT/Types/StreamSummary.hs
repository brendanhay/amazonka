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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of a stream.
--
-- /See:/ 'newStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | A description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stream ARN.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The stream version.
    streamVersion :: Prelude.Maybe Prelude.Natural
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
-- 'description', 'streamSummary_description' - A description of the stream.
--
-- 'streamArn', 'streamSummary_streamArn' - The stream ARN.
--
-- 'streamId', 'streamSummary_streamId' - The stream ID.
--
-- 'streamVersion', 'streamSummary_streamVersion' - The stream version.
newStreamSummary ::
  StreamSummary
newStreamSummary =
  StreamSummary'
    { description = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      streamId = Prelude.Nothing,
      streamVersion = Prelude.Nothing
    }

-- | A description of the stream.
streamSummary_description :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_description = Lens.lens (\StreamSummary' {description} -> description) (\s@StreamSummary' {} a -> s {description = a} :: StreamSummary)

-- | The stream ARN.
streamSummary_streamArn :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_streamArn = Lens.lens (\StreamSummary' {streamArn} -> streamArn) (\s@StreamSummary' {} a -> s {streamArn = a} :: StreamSummary)

-- | The stream ID.
streamSummary_streamId :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_streamId = Lens.lens (\StreamSummary' {streamId} -> streamId) (\s@StreamSummary' {} a -> s {streamId = a} :: StreamSummary)

-- | The stream version.
streamSummary_streamVersion :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Natural)
streamSummary_streamVersion = Lens.lens (\StreamSummary' {streamVersion} -> streamVersion) (\s@StreamSummary' {} a -> s {streamVersion = a} :: StreamSummary)

instance Data.FromJSON StreamSummary where
  parseJSON =
    Data.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "streamArn")
            Prelude.<*> (x Data..:? "streamId")
            Prelude.<*> (x Data..:? "streamVersion")
      )

instance Prelude.Hashable StreamSummary where
  hashWithSalt _salt StreamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` streamArn
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` streamVersion

instance Prelude.NFData StreamSummary where
  rnf StreamSummary' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf streamArn `Prelude.seq`
        Prelude.rnf streamId `Prelude.seq`
          Prelude.rnf streamVersion
