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
-- Module      : Amazonka.IoT.Types.SnsAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SnsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.MessageFormat
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to publish to an Amazon SNS topic.
--
-- /See:/ 'newSnsAction' smart constructor.
data SnsAction = SnsAction'
  { -- | (Optional) The message format of the message to publish. Accepted values
    -- are \"JSON\" and \"RAW\". The default value of the attribute is \"RAW\".
    -- SNS uses this setting to determine if the payload should be parsed and
    -- relevant platform-specific bits of the payload should be extracted. To
    -- read more about SNS message formats, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to
    -- their official documentation.
    messageFormat :: Prelude.Maybe MessageFormat,
    -- | The ARN of the SNS topic.
    targetArn :: Prelude.Text,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageFormat', 'snsAction_messageFormat' - (Optional) The message format of the message to publish. Accepted values
-- are \"JSON\" and \"RAW\". The default value of the attribute is \"RAW\".
-- SNS uses this setting to determine if the payload should be parsed and
-- relevant platform-specific bits of the payload should be extracted. To
-- read more about SNS message formats, see
-- <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to
-- their official documentation.
--
-- 'targetArn', 'snsAction_targetArn' - The ARN of the SNS topic.
--
-- 'roleArn', 'snsAction_roleArn' - The ARN of the IAM role that grants access.
newSnsAction ::
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SnsAction
newSnsAction pTargetArn_ pRoleArn_ =
  SnsAction'
    { messageFormat = Prelude.Nothing,
      targetArn = pTargetArn_,
      roleArn = pRoleArn_
    }

-- | (Optional) The message format of the message to publish. Accepted values
-- are \"JSON\" and \"RAW\". The default value of the attribute is \"RAW\".
-- SNS uses this setting to determine if the payload should be parsed and
-- relevant platform-specific bits of the payload should be extracted. To
-- read more about SNS message formats, see
-- <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to
-- their official documentation.
snsAction_messageFormat :: Lens.Lens' SnsAction (Prelude.Maybe MessageFormat)
snsAction_messageFormat = Lens.lens (\SnsAction' {messageFormat} -> messageFormat) (\s@SnsAction' {} a -> s {messageFormat = a} :: SnsAction)

-- | The ARN of the SNS topic.
snsAction_targetArn :: Lens.Lens' SnsAction Prelude.Text
snsAction_targetArn = Lens.lens (\SnsAction' {targetArn} -> targetArn) (\s@SnsAction' {} a -> s {targetArn = a} :: SnsAction)

-- | The ARN of the IAM role that grants access.
snsAction_roleArn :: Lens.Lens' SnsAction Prelude.Text
snsAction_roleArn = Lens.lens (\SnsAction' {roleArn} -> roleArn) (\s@SnsAction' {} a -> s {roleArn = a} :: SnsAction)

instance Data.FromJSON SnsAction where
  parseJSON =
    Data.withObject
      "SnsAction"
      ( \x ->
          SnsAction'
            Prelude.<$> (x Data..:? "messageFormat")
            Prelude.<*> (x Data..: "targetArn")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable SnsAction where
  hashWithSalt _salt SnsAction' {..} =
    _salt
      `Prelude.hashWithSalt` messageFormat
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SnsAction where
  rnf SnsAction' {..} =
    Prelude.rnf messageFormat
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON SnsAction where
  toJSON SnsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("messageFormat" Data..=) Prelude.<$> messageFormat,
            Prelude.Just ("targetArn" Data..= targetArn),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
