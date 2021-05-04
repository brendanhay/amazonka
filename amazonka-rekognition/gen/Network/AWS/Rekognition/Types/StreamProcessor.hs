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
-- Module      : Network.AWS.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessor where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    status :: Prelude.Maybe StreamProcessorStatus,
    -- | Name of the Amazon Rekognition stream processor.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Current status of the Amazon Rekognition stream processor.
streamProcessor_status :: Lens.Lens' StreamProcessor (Prelude.Maybe StreamProcessorStatus)
streamProcessor_status = Lens.lens (\StreamProcessor' {status} -> status) (\s@StreamProcessor' {} a -> s {status = a} :: StreamProcessor)

-- | Name of the Amazon Rekognition stream processor.
streamProcessor_name :: Lens.Lens' StreamProcessor (Prelude.Maybe Prelude.Text)
streamProcessor_name = Lens.lens (\StreamProcessor' {name} -> name) (\s@StreamProcessor' {} a -> s {name = a} :: StreamProcessor)

instance Prelude.FromJSON StreamProcessor where
  parseJSON =
    Prelude.withObject
      "StreamProcessor"
      ( \x ->
          StreamProcessor'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable StreamProcessor

instance Prelude.NFData StreamProcessor
