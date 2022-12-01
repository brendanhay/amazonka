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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.RecommendationImpact
import Amazonka.SESV2.Types.RecommendationStatus
import Amazonka.SESV2.Types.RecommendationType

-- | A recommendation generated for your account.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The last time the recommendation was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The recommendation impact, with values like @HIGH@ or @LOW@.
    impact :: Prelude.Maybe RecommendationImpact,
    -- | The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
    type' :: Prelude.Maybe RecommendationType,
    -- | The first time this issue was encountered and the recommendation was
    -- generated.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The recommendation status, with values like @OPEN@ or @FIXED@.
    status :: Prelude.Maybe RecommendationStatus,
    -- | The recommendation description \/ disambiguator - e.g. @DKIM1@ and
    -- @DKIM2@ are different recommendations about your DKIM setup.
    description :: Prelude.Maybe Prelude.Text,
    -- | The resource affected by the recommendation, with values like
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    resourceArn :: Prelude.Maybe Prelude.Text
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
-- 'lastUpdatedTimestamp', 'recommendation_lastUpdatedTimestamp' - The last time the recommendation was updated.
--
-- 'impact', 'recommendation_impact' - The recommendation impact, with values like @HIGH@ or @LOW@.
--
-- 'type'', 'recommendation_type' - The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
--
-- 'createdTimestamp', 'recommendation_createdTimestamp' - The first time this issue was encountered and the recommendation was
-- generated.
--
-- 'status', 'recommendation_status' - The recommendation status, with values like @OPEN@ or @FIXED@.
--
-- 'description', 'recommendation_description' - The recommendation description \/ disambiguator - e.g. @DKIM1@ and
-- @DKIM2@ are different recommendations about your DKIM setup.
--
-- 'resourceArn', 'recommendation_resourceArn' - The resource affected by the recommendation, with values like
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      impact = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The last time the recommendation was updated.
recommendation_lastUpdatedTimestamp :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.UTCTime)
recommendation_lastUpdatedTimestamp = Lens.lens (\Recommendation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Recommendation' {} a -> s {lastUpdatedTimestamp = a} :: Recommendation) Prelude.. Lens.mapping Core._Time

-- | The recommendation impact, with values like @HIGH@ or @LOW@.
recommendation_impact :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationImpact)
recommendation_impact = Lens.lens (\Recommendation' {impact} -> impact) (\s@Recommendation' {} a -> s {impact = a} :: Recommendation)

-- | The recommendation type, with values like @DKIM@, @SPF@ or @DMARC@.
recommendation_type :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationType)
recommendation_type = Lens.lens (\Recommendation' {type'} -> type') (\s@Recommendation' {} a -> s {type' = a} :: Recommendation)

-- | The first time this issue was encountered and the recommendation was
-- generated.
recommendation_createdTimestamp :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.UTCTime)
recommendation_createdTimestamp = Lens.lens (\Recommendation' {createdTimestamp} -> createdTimestamp) (\s@Recommendation' {} a -> s {createdTimestamp = a} :: Recommendation) Prelude.. Lens.mapping Core._Time

-- | The recommendation status, with values like @OPEN@ or @FIXED@.
recommendation_status :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationStatus)
recommendation_status = Lens.lens (\Recommendation' {status} -> status) (\s@Recommendation' {} a -> s {status = a} :: Recommendation)

-- | The recommendation description \/ disambiguator - e.g. @DKIM1@ and
-- @DKIM2@ are different recommendations about your DKIM setup.
recommendation_description :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_description = Lens.lens (\Recommendation' {description} -> description) (\s@Recommendation' {} a -> s {description = a} :: Recommendation)

-- | The resource affected by the recommendation, with values like
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
recommendation_resourceArn :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_resourceArn = Lens.lens (\Recommendation' {resourceArn} -> resourceArn) (\s@Recommendation' {} a -> s {resourceArn = a} :: Recommendation)

instance Core.FromJSON Recommendation where
  parseJSON =
    Core.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Impact")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ResourceArn")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf resourceArn
