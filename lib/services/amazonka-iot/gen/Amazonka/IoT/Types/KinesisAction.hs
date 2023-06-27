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
-- Module      : Amazonka.IoT.Types.KinesisAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.KinesisAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to write data to an Amazon Kinesis stream.
--
-- /See:/ 'newKinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { -- | The partition key.
    partitionKey :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
    roleArn :: Prelude.Text,
    -- | The name of the Amazon Kinesis stream.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionKey', 'kinesisAction_partitionKey' - The partition key.
--
-- 'roleArn', 'kinesisAction_roleArn' - The ARN of the IAM role that grants access to the Amazon Kinesis stream.
--
-- 'streamName', 'kinesisAction_streamName' - The name of the Amazon Kinesis stream.
newKinesisAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'streamName'
  Prelude.Text ->
  KinesisAction
newKinesisAction pRoleArn_ pStreamName_ =
  KinesisAction'
    { partitionKey = Prelude.Nothing,
      roleArn = pRoleArn_,
      streamName = pStreamName_
    }

-- | The partition key.
kinesisAction_partitionKey :: Lens.Lens' KinesisAction (Prelude.Maybe Prelude.Text)
kinesisAction_partitionKey = Lens.lens (\KinesisAction' {partitionKey} -> partitionKey) (\s@KinesisAction' {} a -> s {partitionKey = a} :: KinesisAction)

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
kinesisAction_roleArn :: Lens.Lens' KinesisAction Prelude.Text
kinesisAction_roleArn = Lens.lens (\KinesisAction' {roleArn} -> roleArn) (\s@KinesisAction' {} a -> s {roleArn = a} :: KinesisAction)

-- | The name of the Amazon Kinesis stream.
kinesisAction_streamName :: Lens.Lens' KinesisAction Prelude.Text
kinesisAction_streamName = Lens.lens (\KinesisAction' {streamName} -> streamName) (\s@KinesisAction' {} a -> s {streamName = a} :: KinesisAction)

instance Data.FromJSON KinesisAction where
  parseJSON =
    Data.withObject
      "KinesisAction"
      ( \x ->
          KinesisAction'
            Prelude.<$> (x Data..:? "partitionKey")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "streamName")
      )

instance Prelude.Hashable KinesisAction where
  hashWithSalt _salt KinesisAction' {..} =
    _salt
      `Prelude.hashWithSalt` partitionKey
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData KinesisAction where
  rnf KinesisAction' {..} =
    Prelude.rnf partitionKey
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToJSON KinesisAction where
  toJSON KinesisAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("partitionKey" Data..=) Prelude.<$> partitionKey,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("streamName" Data..= streamName)
          ]
      )
