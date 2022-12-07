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
-- Module      : Amazonka.IoT.Types.SqsAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SqsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to publish data to an Amazon SQS queue.
--
-- /See:/ 'newSqsAction' smart constructor.
data SqsAction = SqsAction'
  { -- | Specifies whether to use Base64 encoding.
    useBase64 :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Text,
    -- | The URL of the Amazon SQS queue.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'queueUrl'
  Prelude.Text ->
  SqsAction
newSqsAction pRoleArn_ pQueueUrl_ =
  SqsAction'
    { useBase64 = Prelude.Nothing,
      roleArn = pRoleArn_,
      queueUrl = pQueueUrl_
    }

-- | Specifies whether to use Base64 encoding.
sqsAction_useBase64 :: Lens.Lens' SqsAction (Prelude.Maybe Prelude.Bool)
sqsAction_useBase64 = Lens.lens (\SqsAction' {useBase64} -> useBase64) (\s@SqsAction' {} a -> s {useBase64 = a} :: SqsAction)

-- | The ARN of the IAM role that grants access.
sqsAction_roleArn :: Lens.Lens' SqsAction Prelude.Text
sqsAction_roleArn = Lens.lens (\SqsAction' {roleArn} -> roleArn) (\s@SqsAction' {} a -> s {roleArn = a} :: SqsAction)

-- | The URL of the Amazon SQS queue.
sqsAction_queueUrl :: Lens.Lens' SqsAction Prelude.Text
sqsAction_queueUrl = Lens.lens (\SqsAction' {queueUrl} -> queueUrl) (\s@SqsAction' {} a -> s {queueUrl = a} :: SqsAction)

instance Data.FromJSON SqsAction where
  parseJSON =
    Data.withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            Prelude.<$> (x Data..:? "useBase64")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "queueUrl")
      )

instance Prelude.Hashable SqsAction where
  hashWithSalt _salt SqsAction' {..} =
    _salt `Prelude.hashWithSalt` useBase64
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData SqsAction where
  rnf SqsAction' {..} =
    Prelude.rnf useBase64
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf queueUrl

instance Data.ToJSON SqsAction where
  toJSON SqsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("useBase64" Data..=) Prelude.<$> useBase64,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("queueUrl" Data..= queueUrl)
          ]
      )
