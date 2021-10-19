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
-- Module      : Network.AWS.DynamoDBStreams.Types.Stream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Stream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | A timestamp, in ISO 8601 format, for this stream.
    --
    -- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
    -- because it is possible that a stream from another table might have the
    -- same timestamp. However, the combination of the following three elements
    -- is guaranteed to be unique:
    --
    -- -   the AWS customer ID.
    --
    -- -   the table name
    --
    -- -   the @StreamLabel@
    streamLabel :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Prelude.Maybe Prelude.Text,
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
-- 'streamLabel', 'stream_streamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
--
-- 'streamArn', 'stream_streamArn' - The Amazon Resource Name (ARN) for the stream.
--
-- 'tableName', 'stream_tableName' - The DynamoDB table with which the stream is associated.
newStream ::
  Stream
newStream =
  Stream'
    { streamLabel = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
stream_streamLabel :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamLabel = Lens.lens (\Stream' {streamLabel} -> streamLabel) (\s@Stream' {} a -> s {streamLabel = a} :: Stream)

-- | The Amazon Resource Name (ARN) for the stream.
stream_streamArn :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamArn = Lens.lens (\Stream' {streamArn} -> streamArn) (\s@Stream' {} a -> s {streamArn = a} :: Stream)

-- | The DynamoDB table with which the stream is associated.
stream_tableName :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_tableName = Lens.lens (\Stream' {tableName} -> tableName) (\s@Stream' {} a -> s {tableName = a} :: Stream)

instance Core.FromJSON Stream where
  parseJSON =
    Core.withObject
      "Stream"
      ( \x ->
          Stream'
            Prelude.<$> (x Core..:? "StreamLabel")
            Prelude.<*> (x Core..:? "StreamArn")
            Prelude.<*> (x Core..:? "TableName")
      )

instance Prelude.Hashable Stream

instance Prelude.NFData Stream
