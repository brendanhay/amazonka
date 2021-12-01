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
-- Module      : Amazonka.LookoutMetrics.Types.SNSConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.SNSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the SNS topic to which you want to send your
-- alerts and the IAM role that has access to that topic.
--
-- /See:/ 'newSNSConfiguration' smart constructor.
data SNSConfiguration = SNSConfiguration'
  { -- | The ARN of the IAM role that has access to the target SNS topic.
    roleArn :: Prelude.Text,
    -- | The ARN of the target SNS topic.
    snsTopicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'sNSConfiguration_roleArn' - The ARN of the IAM role that has access to the target SNS topic.
--
-- 'snsTopicArn', 'sNSConfiguration_snsTopicArn' - The ARN of the target SNS topic.
newSNSConfiguration ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'snsTopicArn'
  Prelude.Text ->
  SNSConfiguration
newSNSConfiguration pRoleArn_ pSnsTopicArn_ =
  SNSConfiguration'
    { roleArn = pRoleArn_,
      snsTopicArn = pSnsTopicArn_
    }

-- | The ARN of the IAM role that has access to the target SNS topic.
sNSConfiguration_roleArn :: Lens.Lens' SNSConfiguration Prelude.Text
sNSConfiguration_roleArn = Lens.lens (\SNSConfiguration' {roleArn} -> roleArn) (\s@SNSConfiguration' {} a -> s {roleArn = a} :: SNSConfiguration)

-- | The ARN of the target SNS topic.
sNSConfiguration_snsTopicArn :: Lens.Lens' SNSConfiguration Prelude.Text
sNSConfiguration_snsTopicArn = Lens.lens (\SNSConfiguration' {snsTopicArn} -> snsTopicArn) (\s@SNSConfiguration' {} a -> s {snsTopicArn = a} :: SNSConfiguration)

instance Core.FromJSON SNSConfiguration where
  parseJSON =
    Core.withObject
      "SNSConfiguration"
      ( \x ->
          SNSConfiguration'
            Prelude.<$> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "SnsTopicArn")
      )

instance Prelude.Hashable SNSConfiguration where
  hashWithSalt salt' SNSConfiguration' {..} =
    salt' `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SNSConfiguration where
  rnf SNSConfiguration' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf snsTopicArn

instance Core.ToJSON SNSConfiguration where
  toJSON SNSConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just ("SnsTopicArn" Core..= snsTopicArn)
          ]
      )
