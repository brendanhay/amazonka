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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.SentimentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.SentimentConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.SentimentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for a sentiment
-- analysis task.
--
-- /See:/ 'newSentimentConfiguration' smart constructor.
data SentimentConfiguration = SentimentConfiguration'
  { -- | The name of the rule in the sentiment configuration.
    ruleName :: Prelude.Text,
    -- | The type of sentiment, @POSITIVE@, @NEGATIVE@, or @NEUTRAL@.
    sentimentType :: SentimentType,
    -- | Specifies the analysis interval.
    timePeriod :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SentimentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'sentimentConfiguration_ruleName' - The name of the rule in the sentiment configuration.
--
-- 'sentimentType', 'sentimentConfiguration_sentimentType' - The type of sentiment, @POSITIVE@, @NEGATIVE@, or @NEUTRAL@.
--
-- 'timePeriod', 'sentimentConfiguration_timePeriod' - Specifies the analysis interval.
newSentimentConfiguration ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'sentimentType'
  SentimentType ->
  -- | 'timePeriod'
  Prelude.Natural ->
  SentimentConfiguration
newSentimentConfiguration
  pRuleName_
  pSentimentType_
  pTimePeriod_ =
    SentimentConfiguration'
      { ruleName = pRuleName_,
        sentimentType = pSentimentType_,
        timePeriod = pTimePeriod_
      }

-- | The name of the rule in the sentiment configuration.
sentimentConfiguration_ruleName :: Lens.Lens' SentimentConfiguration Prelude.Text
sentimentConfiguration_ruleName = Lens.lens (\SentimentConfiguration' {ruleName} -> ruleName) (\s@SentimentConfiguration' {} a -> s {ruleName = a} :: SentimentConfiguration)

-- | The type of sentiment, @POSITIVE@, @NEGATIVE@, or @NEUTRAL@.
sentimentConfiguration_sentimentType :: Lens.Lens' SentimentConfiguration SentimentType
sentimentConfiguration_sentimentType = Lens.lens (\SentimentConfiguration' {sentimentType} -> sentimentType) (\s@SentimentConfiguration' {} a -> s {sentimentType = a} :: SentimentConfiguration)

-- | Specifies the analysis interval.
sentimentConfiguration_timePeriod :: Lens.Lens' SentimentConfiguration Prelude.Natural
sentimentConfiguration_timePeriod = Lens.lens (\SentimentConfiguration' {timePeriod} -> timePeriod) (\s@SentimentConfiguration' {} a -> s {timePeriod = a} :: SentimentConfiguration)

instance Data.FromJSON SentimentConfiguration where
  parseJSON =
    Data.withObject
      "SentimentConfiguration"
      ( \x ->
          SentimentConfiguration'
            Prelude.<$> (x Data..: "RuleName")
            Prelude.<*> (x Data..: "SentimentType")
            Prelude.<*> (x Data..: "TimePeriod")
      )

instance Prelude.Hashable SentimentConfiguration where
  hashWithSalt _salt SentimentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` sentimentType
      `Prelude.hashWithSalt` timePeriod

instance Prelude.NFData SentimentConfiguration where
  rnf SentimentConfiguration' {..} =
    Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf sentimentType
      `Prelude.seq` Prelude.rnf timePeriod

instance Data.ToJSON SentimentConfiguration where
  toJSON SentimentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleName" Data..= ruleName),
            Prelude.Just ("SentimentType" Data..= sentimentType),
            Prelude.Just ("TimePeriod" Data..= timePeriod)
          ]
      )
