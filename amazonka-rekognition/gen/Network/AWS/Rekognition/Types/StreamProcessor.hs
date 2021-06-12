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
-- Module      : Network.AWS.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessor where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.StreamProcessorStatus

-- | An object that recognizes faces in a streaming video. An Amazon
-- Rekognition stream processor is created by a call to
-- CreateStreamProcessor. The request parameters for
-- @CreateStreamProcessor@ describe the Kinesis video stream source for the
-- streaming video, face recognition parameters, and where to stream the
-- analysis resullts.
--
-- /See:/ 'newStreamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { -- | Current status of the Amazon Rekognition stream processor.
    status :: Core.Maybe StreamProcessorStatus,
    -- | Name of the Amazon Rekognition stream processor.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'streamProcessor_status' - Current status of the Amazon Rekognition stream processor.
--
-- 'name', 'streamProcessor_name' - Name of the Amazon Rekognition stream processor.
newStreamProcessor ::
  StreamProcessor
newStreamProcessor =
  StreamProcessor'
    { status = Core.Nothing,
      name = Core.Nothing
    }

-- | Current status of the Amazon Rekognition stream processor.
streamProcessor_status :: Lens.Lens' StreamProcessor (Core.Maybe StreamProcessorStatus)
streamProcessor_status = Lens.lens (\StreamProcessor' {status} -> status) (\s@StreamProcessor' {} a -> s {status = a} :: StreamProcessor)

-- | Name of the Amazon Rekognition stream processor.
streamProcessor_name :: Lens.Lens' StreamProcessor (Core.Maybe Core.Text)
streamProcessor_name = Lens.lens (\StreamProcessor' {name} -> name) (\s@StreamProcessor' {} a -> s {name = a} :: StreamProcessor)

instance Core.FromJSON StreamProcessor where
  parseJSON =
    Core.withObject
      "StreamProcessor"
      ( \x ->
          StreamProcessor'
            Core.<$> (x Core..:? "Status") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable StreamProcessor

instance Core.NFData StreamProcessor
