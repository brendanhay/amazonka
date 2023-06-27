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
-- Module      : Amazonka.DynamoDBStreams.Types.Stream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.Stream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp, in ISO 8601 format, for this stream.
    --
    -- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
    -- because it is possible that a stream from another table might have the
    -- same timestamp. However, the combination of the following three elements
    -- is guaranteed to be unique:
    --
    -- -   the Amazon Web Services customer ID.
    --
    -- -   the table name
    --
    -- -   the @StreamLabel@
    streamLabel :: Prelude.Maybe Prelude.Text,
    -- | The DynamoDB table with which the stream is associated.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamArn', 'stream_streamArn' - The Amazon Resource Name (ARN) for the stream.
--
-- 'streamLabel', 'stream_streamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the Amazon Web Services customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
--
-- 'tableName', 'stream_tableName' - The DynamoDB table with which the stream is associated.
newStream ::
  Stream
newStream =
  Stream'
    { streamArn = Prelude.Nothing,
      streamLabel = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the stream.
stream_streamArn :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamArn = Lens.lens (\Stream' {streamArn} -> streamArn) (\s@Stream' {} a -> s {streamArn = a} :: Stream)

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the Amazon Web Services customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
stream_streamLabel :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamLabel = Lens.lens (\Stream' {streamLabel} -> streamLabel) (\s@Stream' {} a -> s {streamLabel = a} :: Stream)

-- | The DynamoDB table with which the stream is associated.
stream_tableName :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_tableName = Lens.lens (\Stream' {tableName} -> tableName) (\s@Stream' {} a -> s {tableName = a} :: Stream)

instance Data.FromJSON Stream where
  parseJSON =
    Data.withObject
      "Stream"
      ( \x ->
          Stream'
            Prelude.<$> (x Data..:? "StreamArn")
            Prelude.<*> (x Data..:? "StreamLabel")
            Prelude.<*> (x Data..:? "TableName")
      )

instance Prelude.Hashable Stream where
  hashWithSalt _salt Stream' {..} =
    _salt
      `Prelude.hashWithSalt` streamArn
      `Prelude.hashWithSalt` streamLabel
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Stream where
  rnf Stream' {..} =
    Prelude.rnf streamArn
      `Prelude.seq` Prelude.rnf streamLabel
      `Prelude.seq` Prelude.rnf tableName
