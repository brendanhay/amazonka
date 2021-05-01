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
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newKinesisStreamingDestinationInput' smart constructor.
data KinesisStreamingDestinationInput = KinesisStreamingDestinationInput'
  { -- | The name of the DynamoDB table.
    tableName :: Prelude.Text,
    -- | The ARN for a Kinesis data stream.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamingDestinationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'kinesisStreamingDestinationInput_tableName' - The name of the DynamoDB table.
--
-- 'streamArn', 'kinesisStreamingDestinationInput_streamArn' - The ARN for a Kinesis data stream.
newKinesisStreamingDestinationInput ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'streamArn'
  Prelude.Text ->
  KinesisStreamingDestinationInput
newKinesisStreamingDestinationInput
  pTableName_
  pStreamArn_ =
    KinesisStreamingDestinationInput'
      { tableName =
          pTableName_,
        streamArn = pStreamArn_
      }

-- | The name of the DynamoDB table.
kinesisStreamingDestinationInput_tableName :: Lens.Lens' KinesisStreamingDestinationInput Prelude.Text
kinesisStreamingDestinationInput_tableName = Lens.lens (\KinesisStreamingDestinationInput' {tableName} -> tableName) (\s@KinesisStreamingDestinationInput' {} a -> s {tableName = a} :: KinesisStreamingDestinationInput)

-- | The ARN for a Kinesis data stream.
kinesisStreamingDestinationInput_streamArn :: Lens.Lens' KinesisStreamingDestinationInput Prelude.Text
kinesisStreamingDestinationInput_streamArn = Lens.lens (\KinesisStreamingDestinationInput' {streamArn} -> streamArn) (\s@KinesisStreamingDestinationInput' {} a -> s {streamArn = a} :: KinesisStreamingDestinationInput)

instance
  Prelude.Hashable
    KinesisStreamingDestinationInput

instance
  Prelude.NFData
    KinesisStreamingDestinationInput

instance
  Prelude.ToJSON
    KinesisStreamingDestinationInput
  where
  toJSON KinesisStreamingDestinationInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just ("StreamArn" Prelude..= streamArn)
          ]
      )
