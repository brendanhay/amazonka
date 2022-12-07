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
-- Module      : Amazonka.Rekognition.Types.StreamProcessingStartSelector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessingStartSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.KinesisVideoStreamStartSelector

-- | This is a required parameter for label detection stream processors and
-- should not be used to start a face search stream processor.
--
-- /See:/ 'newStreamProcessingStartSelector' smart constructor.
data StreamProcessingStartSelector = StreamProcessingStartSelector'
  { -- | Specifies the starting point in the stream to start processing. This can
    -- be done with a producer timestamp or a fragment number in a Kinesis
    -- stream.
    kVSStreamStartSelector :: Prelude.Maybe KinesisVideoStreamStartSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessingStartSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kVSStreamStartSelector', 'streamProcessingStartSelector_kVSStreamStartSelector' - Specifies the starting point in the stream to start processing. This can
-- be done with a producer timestamp or a fragment number in a Kinesis
-- stream.
newStreamProcessingStartSelector ::
  StreamProcessingStartSelector
newStreamProcessingStartSelector =
  StreamProcessingStartSelector'
    { kVSStreamStartSelector =
        Prelude.Nothing
    }

-- | Specifies the starting point in the stream to start processing. This can
-- be done with a producer timestamp or a fragment number in a Kinesis
-- stream.
streamProcessingStartSelector_kVSStreamStartSelector :: Lens.Lens' StreamProcessingStartSelector (Prelude.Maybe KinesisVideoStreamStartSelector)
streamProcessingStartSelector_kVSStreamStartSelector = Lens.lens (\StreamProcessingStartSelector' {kVSStreamStartSelector} -> kVSStreamStartSelector) (\s@StreamProcessingStartSelector' {} a -> s {kVSStreamStartSelector = a} :: StreamProcessingStartSelector)

instance
  Prelude.Hashable
    StreamProcessingStartSelector
  where
  hashWithSalt _salt StreamProcessingStartSelector' {..} =
    _salt `Prelude.hashWithSalt` kVSStreamStartSelector

instance Prelude.NFData StreamProcessingStartSelector where
  rnf StreamProcessingStartSelector' {..} =
    Prelude.rnf kVSStreamStartSelector

instance Data.ToJSON StreamProcessingStartSelector where
  toJSON StreamProcessingStartSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KVSStreamStartSelector" Data..=)
              Prelude.<$> kVSStreamStartSelector
          ]
      )
