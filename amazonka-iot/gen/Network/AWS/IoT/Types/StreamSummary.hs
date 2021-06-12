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
-- Module      : Network.AWS.IoT.Types.StreamSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of a stream.
--
-- /See:/ 'newStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | The stream version.
    streamVersion :: Core.Maybe Core.Natural,
    -- | The stream ID.
    streamId :: Core.Maybe Core.Text,
    -- | The stream ARN.
    streamArn :: Core.Maybe Core.Text,
    -- | A description of the stream.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamVersion', 'streamSummary_streamVersion' - The stream version.
--
-- 'streamId', 'streamSummary_streamId' - The stream ID.
--
-- 'streamArn', 'streamSummary_streamArn' - The stream ARN.
--
-- 'description', 'streamSummary_description' - A description of the stream.
newStreamSummary ::
  StreamSummary
newStreamSummary =
  StreamSummary'
    { streamVersion = Core.Nothing,
      streamId = Core.Nothing,
      streamArn = Core.Nothing,
      description = Core.Nothing
    }

-- | The stream version.
streamSummary_streamVersion :: Lens.Lens' StreamSummary (Core.Maybe Core.Natural)
streamSummary_streamVersion = Lens.lens (\StreamSummary' {streamVersion} -> streamVersion) (\s@StreamSummary' {} a -> s {streamVersion = a} :: StreamSummary)

-- | The stream ID.
streamSummary_streamId :: Lens.Lens' StreamSummary (Core.Maybe Core.Text)
streamSummary_streamId = Lens.lens (\StreamSummary' {streamId} -> streamId) (\s@StreamSummary' {} a -> s {streamId = a} :: StreamSummary)

-- | The stream ARN.
streamSummary_streamArn :: Lens.Lens' StreamSummary (Core.Maybe Core.Text)
streamSummary_streamArn = Lens.lens (\StreamSummary' {streamArn} -> streamArn) (\s@StreamSummary' {} a -> s {streamArn = a} :: StreamSummary)

-- | A description of the stream.
streamSummary_description :: Lens.Lens' StreamSummary (Core.Maybe Core.Text)
streamSummary_description = Lens.lens (\StreamSummary' {description} -> description) (\s@StreamSummary' {} a -> s {description = a} :: StreamSummary)

instance Core.FromJSON StreamSummary where
  parseJSON =
    Core.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Core.<$> (x Core..:? "streamVersion")
            Core.<*> (x Core..:? "streamId")
            Core.<*> (x Core..:? "streamArn")
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable StreamSummary

instance Core.NFData StreamSummary
