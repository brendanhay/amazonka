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
-- Module      : Amazonka.Kinesis.Types.StreamModeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.StreamModeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.StreamMode
import qualified Amazonka.Prelude as Prelude

-- | Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
--
-- /See:/ 'newStreamModeDetails' smart constructor.
data StreamModeDetails = StreamModeDetails'
  { -- | Specifies the capacity mode to which you want to set your data stream.
    -- Currently, in Kinesis Data Streams, you can choose between an
    -- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
    -- data streams.
    streamMode :: StreamMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamModeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamMode', 'streamModeDetails_streamMode' - Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
newStreamModeDetails ::
  -- | 'streamMode'
  StreamMode ->
  StreamModeDetails
newStreamModeDetails pStreamMode_ =
  StreamModeDetails' {streamMode = pStreamMode_}

-- | Specifies the capacity mode to which you want to set your data stream.
-- Currently, in Kinesis Data Streams, you can choose between an
-- __on-demand__ capacity mode and a __provisioned__ capacity mode for your
-- data streams.
streamModeDetails_streamMode :: Lens.Lens' StreamModeDetails StreamMode
streamModeDetails_streamMode = Lens.lens (\StreamModeDetails' {streamMode} -> streamMode) (\s@StreamModeDetails' {} a -> s {streamMode = a} :: StreamModeDetails)

instance Data.FromJSON StreamModeDetails where
  parseJSON =
    Data.withObject
      "StreamModeDetails"
      ( \x ->
          StreamModeDetails'
            Prelude.<$> (x Data..: "StreamMode")
      )

instance Prelude.Hashable StreamModeDetails where
  hashWithSalt _salt StreamModeDetails' {..} =
    _salt `Prelude.hashWithSalt` streamMode

instance Prelude.NFData StreamModeDetails where
  rnf StreamModeDetails' {..} = Prelude.rnf streamMode

instance Data.ToJSON StreamModeDetails where
  toJSON StreamModeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StreamMode" Data..= streamMode)]
      )
