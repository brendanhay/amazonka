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
-- Module      : Amazonka.ECR.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the recommended course of action to remediate the finding.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The recommended course of action to remediate the finding.
    text :: Prelude.Maybe Prelude.Text,
    -- | The URL address to the CVE remediation recommendations.
    url :: Prelude.Maybe Prelude.Text
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
-- 'text', 'recommendation_text' - The recommended course of action to remediate the finding.
--
-- 'url', 'recommendation_url' - The URL address to the CVE remediation recommendations.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { text = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The recommended course of action to remediate the finding.
recommendation_text :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_text = Lens.lens (\Recommendation' {text} -> text) (\s@Recommendation' {} a -> s {text = a} :: Recommendation)

-- | The URL address to the CVE remediation recommendations.
recommendation_url :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_url = Lens.lens (\Recommendation' {url} -> url) (\s@Recommendation' {} a -> s {url = a} :: Recommendation)

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..:? "text")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` url

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf text `Prelude.seq` Prelude.rnf url
