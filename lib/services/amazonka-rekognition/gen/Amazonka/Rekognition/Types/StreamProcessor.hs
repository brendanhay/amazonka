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
-- Module      : Amazonka.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.StreamProcessorStatus

-- | An object that recognizes faces or labels in a streaming video. An
-- Amazon Rekognition stream processor is created by a call to
-- CreateStreamProcessor. The request parameters for
-- @CreateStreamProcessor@ describe the Kinesis video stream source for the
-- streaming video, face recognition parameters, and where to stream the
-- analysis resullts.
--
-- /See:/ 'newStreamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { -- | Name of the Amazon Rekognition stream processor.
    name :: Prelude.Maybe Prelude.Text,
    -- | Current status of the Amazon Rekognition stream processor.
    status :: Prelude.Maybe StreamProcessorStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'streamProcessor_name' - Name of the Amazon Rekognition stream processor.
--
-- 'status', 'streamProcessor_status' - Current status of the Amazon Rekognition stream processor.
newStreamProcessor ::
  StreamProcessor
newStreamProcessor =
  StreamProcessor'
    { name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Name of the Amazon Rekognition stream processor.
streamProcessor_name :: Lens.Lens' StreamProcessor (Prelude.Maybe Prelude.Text)
streamProcessor_name = Lens.lens (\StreamProcessor' {name} -> name) (\s@StreamProcessor' {} a -> s {name = a} :: StreamProcessor)

-- | Current status of the Amazon Rekognition stream processor.
streamProcessor_status :: Lens.Lens' StreamProcessor (Prelude.Maybe StreamProcessorStatus)
streamProcessor_status = Lens.lens (\StreamProcessor' {status} -> status) (\s@StreamProcessor' {} a -> s {status = a} :: StreamProcessor)

instance Data.FromJSON StreamProcessor where
  parseJSON =
    Data.withObject
      "StreamProcessor"
      ( \x ->
          StreamProcessor'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable StreamProcessor where
  hashWithSalt _salt StreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData StreamProcessor where
  rnf StreamProcessor' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status
