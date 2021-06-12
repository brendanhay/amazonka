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
-- Module      : Network.AWS.IoT.Types.SqsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SqsAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action to publish data to an Amazon SQS queue.
--
-- /See:/ 'newSqsAction' smart constructor.
data SqsAction = SqsAction'
  { -- | Specifies whether to use Base64 encoding.
    useBase64 :: Core.Maybe Core.Bool,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Core.Text,
    -- | The URL of the Amazon SQS queue.
    queueUrl :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SqsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useBase64', 'sqsAction_useBase64' - Specifies whether to use Base64 encoding.
--
-- 'roleArn', 'sqsAction_roleArn' - The ARN of the IAM role that grants access.
--
-- 'queueUrl', 'sqsAction_queueUrl' - The URL of the Amazon SQS queue.
newSqsAction ::
  -- | 'roleArn'
  Core.Text ->
  -- | 'queueUrl'
  Core.Text ->
  SqsAction
newSqsAction pRoleArn_ pQueueUrl_ =
  SqsAction'
    { useBase64 = Core.Nothing,
      roleArn = pRoleArn_,
      queueUrl = pQueueUrl_
    }

-- | Specifies whether to use Base64 encoding.
sqsAction_useBase64 :: Lens.Lens' SqsAction (Core.Maybe Core.Bool)
sqsAction_useBase64 = Lens.lens (\SqsAction' {useBase64} -> useBase64) (\s@SqsAction' {} a -> s {useBase64 = a} :: SqsAction)

-- | The ARN of the IAM role that grants access.
sqsAction_roleArn :: Lens.Lens' SqsAction Core.Text
sqsAction_roleArn = Lens.lens (\SqsAction' {roleArn} -> roleArn) (\s@SqsAction' {} a -> s {roleArn = a} :: SqsAction)

-- | The URL of the Amazon SQS queue.
sqsAction_queueUrl :: Lens.Lens' SqsAction Core.Text
sqsAction_queueUrl = Lens.lens (\SqsAction' {queueUrl} -> queueUrl) (\s@SqsAction' {} a -> s {queueUrl = a} :: SqsAction)

instance Core.FromJSON SqsAction where
  parseJSON =
    Core.withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            Core.<$> (x Core..:? "useBase64")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "queueUrl")
      )

instance Core.Hashable SqsAction

instance Core.NFData SqsAction

instance Core.ToJSON SqsAction where
  toJSON SqsAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("useBase64" Core..=) Core.<$> useBase64,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("queueUrl" Core..= queueUrl)
          ]
      )
