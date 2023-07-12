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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.SNSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.SnsFormat
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the SNS topic to which you want to send your
-- alerts and the IAM role that has access to that topic.
--
-- /See:/ 'newSNSConfiguration' smart constructor.
data SNSConfiguration = SNSConfiguration'
  { -- | The format of the SNS topic.
    --
    -- -   @JSON@ – Send JSON alerts with an anomaly ID and a link to the
    --     anomaly detail page. This is the default.
    --
    -- -   @LONG_TEXT@ – Send human-readable alerts with information about the
    --     impacted timeseries and a link to the anomaly detail page. We
    --     recommend this for email.
    --
    -- -   @SHORT_TEXT@ – Send human-readable alerts with a link to the anomaly
    --     detail page. We recommend this for SMS.
    snsFormat :: Prelude.Maybe SnsFormat,
    -- | The ARN of the IAM role that has access to the target SNS topic.
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
-- 'snsFormat', 'sNSConfiguration_snsFormat' - The format of the SNS topic.
--
-- -   @JSON@ – Send JSON alerts with an anomaly ID and a link to the
--     anomaly detail page. This is the default.
--
-- -   @LONG_TEXT@ – Send human-readable alerts with information about the
--     impacted timeseries and a link to the anomaly detail page. We
--     recommend this for email.
--
-- -   @SHORT_TEXT@ – Send human-readable alerts with a link to the anomaly
--     detail page. We recommend this for SMS.
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
    { snsFormat = Prelude.Nothing,
      roleArn = pRoleArn_,
      snsTopicArn = pSnsTopicArn_
    }

-- | The format of the SNS topic.
--
-- -   @JSON@ – Send JSON alerts with an anomaly ID and a link to the
--     anomaly detail page. This is the default.
--
-- -   @LONG_TEXT@ – Send human-readable alerts with information about the
--     impacted timeseries and a link to the anomaly detail page. We
--     recommend this for email.
--
-- -   @SHORT_TEXT@ – Send human-readable alerts with a link to the anomaly
--     detail page. We recommend this for SMS.
sNSConfiguration_snsFormat :: Lens.Lens' SNSConfiguration (Prelude.Maybe SnsFormat)
sNSConfiguration_snsFormat = Lens.lens (\SNSConfiguration' {snsFormat} -> snsFormat) (\s@SNSConfiguration' {} a -> s {snsFormat = a} :: SNSConfiguration)

-- | The ARN of the IAM role that has access to the target SNS topic.
sNSConfiguration_roleArn :: Lens.Lens' SNSConfiguration Prelude.Text
sNSConfiguration_roleArn = Lens.lens (\SNSConfiguration' {roleArn} -> roleArn) (\s@SNSConfiguration' {} a -> s {roleArn = a} :: SNSConfiguration)

-- | The ARN of the target SNS topic.
sNSConfiguration_snsTopicArn :: Lens.Lens' SNSConfiguration Prelude.Text
sNSConfiguration_snsTopicArn = Lens.lens (\SNSConfiguration' {snsTopicArn} -> snsTopicArn) (\s@SNSConfiguration' {} a -> s {snsTopicArn = a} :: SNSConfiguration)

instance Data.FromJSON SNSConfiguration where
  parseJSON =
    Data.withObject
      "SNSConfiguration"
      ( \x ->
          SNSConfiguration'
            Prelude.<$> (x Data..:? "SnsFormat")
            Prelude.<*> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "SnsTopicArn")
      )

instance Prelude.Hashable SNSConfiguration where
  hashWithSalt _salt SNSConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` snsFormat
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` snsTopicArn

instance Prelude.NFData SNSConfiguration where
  rnf SNSConfiguration' {..} =
    Prelude.rnf snsFormat
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf snsTopicArn

instance Data.ToJSON SNSConfiguration where
  toJSON SNSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SnsFormat" Data..=) Prelude.<$> snsFormat,
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("SnsTopicArn" Data..= snsTopicArn)
          ]
      )
