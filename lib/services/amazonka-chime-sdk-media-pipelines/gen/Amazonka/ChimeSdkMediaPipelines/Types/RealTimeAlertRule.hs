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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRule where

import Amazonka.ChimeSdkMediaPipelines.Types.IssueDetectionConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KeywordMatchConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRuleType
import Amazonka.ChimeSdkMediaPipelines.Types.SentimentConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the words or phrases that trigger an alert.
--
-- /See:/ 'newRealTimeAlertRule' smart constructor.
data RealTimeAlertRule = RealTimeAlertRule'
  { -- | Specifies the issue detection settings for a real-time alert rule.
    issueDetectionConfiguration :: Prelude.Maybe IssueDetectionConfiguration,
    -- | Specifies the settings for matching the keywords in a real-time alert
    -- rule.
    keywordMatchConfiguration :: Prelude.Maybe KeywordMatchConfiguration,
    -- | Specifies the settings for predicting sentiment in a real-time alert
    -- rule.
    sentimentConfiguration :: Prelude.Maybe SentimentConfiguration,
    -- | The type of alert rule.
    type' :: RealTimeAlertRuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealTimeAlertRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issueDetectionConfiguration', 'realTimeAlertRule_issueDetectionConfiguration' - Specifies the issue detection settings for a real-time alert rule.
--
-- 'keywordMatchConfiguration', 'realTimeAlertRule_keywordMatchConfiguration' - Specifies the settings for matching the keywords in a real-time alert
-- rule.
--
-- 'sentimentConfiguration', 'realTimeAlertRule_sentimentConfiguration' - Specifies the settings for predicting sentiment in a real-time alert
-- rule.
--
-- 'type'', 'realTimeAlertRule_type' - The type of alert rule.
newRealTimeAlertRule ::
  -- | 'type''
  RealTimeAlertRuleType ->
  RealTimeAlertRule
newRealTimeAlertRule pType_ =
  RealTimeAlertRule'
    { issueDetectionConfiguration =
        Prelude.Nothing,
      keywordMatchConfiguration = Prelude.Nothing,
      sentimentConfiguration = Prelude.Nothing,
      type' = pType_
    }

-- | Specifies the issue detection settings for a real-time alert rule.
realTimeAlertRule_issueDetectionConfiguration :: Lens.Lens' RealTimeAlertRule (Prelude.Maybe IssueDetectionConfiguration)
realTimeAlertRule_issueDetectionConfiguration = Lens.lens (\RealTimeAlertRule' {issueDetectionConfiguration} -> issueDetectionConfiguration) (\s@RealTimeAlertRule' {} a -> s {issueDetectionConfiguration = a} :: RealTimeAlertRule)

-- | Specifies the settings for matching the keywords in a real-time alert
-- rule.
realTimeAlertRule_keywordMatchConfiguration :: Lens.Lens' RealTimeAlertRule (Prelude.Maybe KeywordMatchConfiguration)
realTimeAlertRule_keywordMatchConfiguration = Lens.lens (\RealTimeAlertRule' {keywordMatchConfiguration} -> keywordMatchConfiguration) (\s@RealTimeAlertRule' {} a -> s {keywordMatchConfiguration = a} :: RealTimeAlertRule)

-- | Specifies the settings for predicting sentiment in a real-time alert
-- rule.
realTimeAlertRule_sentimentConfiguration :: Lens.Lens' RealTimeAlertRule (Prelude.Maybe SentimentConfiguration)
realTimeAlertRule_sentimentConfiguration = Lens.lens (\RealTimeAlertRule' {sentimentConfiguration} -> sentimentConfiguration) (\s@RealTimeAlertRule' {} a -> s {sentimentConfiguration = a} :: RealTimeAlertRule)

-- | The type of alert rule.
realTimeAlertRule_type :: Lens.Lens' RealTimeAlertRule RealTimeAlertRuleType
realTimeAlertRule_type = Lens.lens (\RealTimeAlertRule' {type'} -> type') (\s@RealTimeAlertRule' {} a -> s {type' = a} :: RealTimeAlertRule)

instance Data.FromJSON RealTimeAlertRule where
  parseJSON =
    Data.withObject
      "RealTimeAlertRule"
      ( \x ->
          RealTimeAlertRule'
            Prelude.<$> (x Data..:? "IssueDetectionConfiguration")
            Prelude.<*> (x Data..:? "KeywordMatchConfiguration")
            Prelude.<*> (x Data..:? "SentimentConfiguration")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable RealTimeAlertRule where
  hashWithSalt _salt RealTimeAlertRule' {..} =
    _salt
      `Prelude.hashWithSalt` issueDetectionConfiguration
      `Prelude.hashWithSalt` keywordMatchConfiguration
      `Prelude.hashWithSalt` sentimentConfiguration
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RealTimeAlertRule where
  rnf RealTimeAlertRule' {..} =
    Prelude.rnf issueDetectionConfiguration
      `Prelude.seq` Prelude.rnf keywordMatchConfiguration
      `Prelude.seq` Prelude.rnf sentimentConfiguration
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON RealTimeAlertRule where
  toJSON RealTimeAlertRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IssueDetectionConfiguration" Data..=)
              Prelude.<$> issueDetectionConfiguration,
            ("KeywordMatchConfiguration" Data..=)
              Prelude.<$> keywordMatchConfiguration,
            ("SentimentConfiguration" Data..=)
              Prelude.<$> sentimentConfiguration,
            Prelude.Just ("Type" Data..= type')
          ]
      )
