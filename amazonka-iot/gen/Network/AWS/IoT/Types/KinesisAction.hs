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
-- Module      : Network.AWS.IoT.Types.KinesisAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KinesisAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action to write data to an Amazon Kinesis stream.
--
-- /See:/ 'newKinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { -- | The partition key.
    partitionKey :: Core.Maybe Core.Text,
    -- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
    roleArn :: Core.Text,
    -- | The name of the Amazon Kinesis stream.
    streamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'streamName'
  Core.Text ->
  KinesisAction
newKinesisAction pRoleArn_ pStreamName_ =
  KinesisAction'
    { partitionKey = Core.Nothing,
      roleArn = pRoleArn_,
      streamName = pStreamName_
    }

-- | The partition key.
kinesisAction_partitionKey :: Lens.Lens' KinesisAction (Core.Maybe Core.Text)
kinesisAction_partitionKey = Lens.lens (\KinesisAction' {partitionKey} -> partitionKey) (\s@KinesisAction' {} a -> s {partitionKey = a} :: KinesisAction)

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
kinesisAction_roleArn :: Lens.Lens' KinesisAction Core.Text
kinesisAction_roleArn = Lens.lens (\KinesisAction' {roleArn} -> roleArn) (\s@KinesisAction' {} a -> s {roleArn = a} :: KinesisAction)

-- | The name of the Amazon Kinesis stream.
kinesisAction_streamName :: Lens.Lens' KinesisAction Core.Text
kinesisAction_streamName = Lens.lens (\KinesisAction' {streamName} -> streamName) (\s@KinesisAction' {} a -> s {streamName = a} :: KinesisAction)

instance Core.FromJSON KinesisAction where
  parseJSON =
    Core.withObject
      "KinesisAction"
      ( \x ->
          KinesisAction'
            Core.<$> (x Core..:? "partitionKey")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "streamName")
      )

instance Core.Hashable KinesisAction

instance Core.NFData KinesisAction

instance Core.ToJSON KinesisAction where
  toJSON KinesisAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("partitionKey" Core..=) Core.<$> partitionKey,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("streamName" Core..= streamName)
          ]
      )
