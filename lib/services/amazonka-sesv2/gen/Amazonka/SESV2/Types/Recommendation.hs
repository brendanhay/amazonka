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
-- Module      : Amazonka.SESV2.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.RecommendationImpact
import Amazonka.SESV2.Types.RecommendationStatus
import Amazonka.SESV2.Types.RecommendationType

-- | A recommendation generated for your account.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The first time this issue was encountered and the recommendation was
    -- generated.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The recommendation description \/ disambiguator - e.g. @DKIM1@ and
    -- @DKIM2@ are different recommendations about your DKIM setup.
    description :: Prelude.Maybe Prelude.Text,
    -- | The recommendation impact, with values like @HIGH@ or @LOW@.
    impact :: Prelude.Maybe RecommendationImpact,
    -- | The last time the recommendation was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The resource affected by the recommendation, with values like
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The recommendation status, with values like @OPEN@ or @FIXED@.
    status :: Prelude.Maybe RecommendationStatus,
    -- | The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
    type' :: Prelude.Maybe RecommendationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'recommendation_createdTimestamp' - The first time this issue was encountered and the recommendation was
-- generated.
--
-- 'description', 'recommendation_description' - The recommendation description \/ disambiguator - e.g. @DKIM1@ and
-- @DKIM2@ are different recommendations about your DKIM setup.
--
-- 'impact', 'recommendation_impact' - The recommendation impact, with values like @HIGH@ or @LOW@.
--
-- 'lastUpdatedTimestamp', 'recommendation_lastUpdatedTimestamp' - The last time the recommendation was updated.
--
-- 'resourceArn', 'recommendation_resourceArn' - The resource affected by the recommendation, with values like
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- 'status', 'recommendation_status' - The recommendation status, with values like @OPEN@ or @FIXED@.
--
-- 'type'', 'recommendation_type' - The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { createdTimestamp = Prelude.Nothing,
      description = Prelude.Nothing,
      impact = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The first time this issue was encountered and the recommendation was
-- generated.
recommendation_createdTimestamp :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.UTCTime)
recommendation_createdTimestamp = Lens.lens (\Recommendation' {createdTimestamp} -> createdTimestamp) (\s@Recommendation' {} a -> s {createdTimestamp = a} :: Recommendation) Prelude.. Lens.mapping Data._Time

-- | The recommendation description \/ disambiguator - e.g. @DKIM1@ and
-- @DKIM2@ are different recommendations about your DKIM setup.
recommendation_description :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_description = Lens.lens (\Recommendation' {description} -> description) (\s@Recommendation' {} a -> s {description = a} :: Recommendation)

-- | The recommendation impact, with values like @HIGH@ or @LOW@.
recommendation_impact :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationImpact)
recommendation_impact = Lens.lens (\Recommendation' {impact} -> impact) (\s@Recommendation' {} a -> s {impact = a} :: Recommendation)

-- | The last time the recommendation was updated.
recommendation_lastUpdatedTimestamp :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.UTCTime)
recommendation_lastUpdatedTimestamp = Lens.lens (\Recommendation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Recommendation' {} a -> s {lastUpdatedTimestamp = a} :: Recommendation) Prelude.. Lens.mapping Data._Time

-- | The resource affected by the recommendation, with values like
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
recommendation_resourceArn :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_resourceArn = Lens.lens (\Recommendation' {resourceArn} -> resourceArn) (\s@Recommendation' {} a -> s {resourceArn = a} :: Recommendation)

-- | The recommendation status, with values like @OPEN@ or @FIXED@.
recommendation_status :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationStatus)
recommendation_status = Lens.lens (\Recommendation' {status} -> status) (\s@Recommendation' {} a -> s {status = a} :: Recommendation)

-- | The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
recommendation_type :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationType)
recommendation_type = Lens.lens (\Recommendation' {type'} -> type') (\s@Recommendation' {} a -> s {type' = a} :: Recommendation)

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Impact")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
