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
-- Module      : Amazonka.ImageBuilder.Types.RemediationRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.RemediationRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the recommended course of action to remediate the finding.
--
-- /See:/ 'newRemediationRecommendation' smart constructor.
data RemediationRecommendation = RemediationRecommendation'
  { -- | The recommended course of action to remediate the finding.
    text :: Prelude.Maybe Prelude.Text,
    -- | A link to more information about the recommended remediation for this
    -- vulnerability.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'remediationRecommendation_text' - The recommended course of action to remediate the finding.
--
-- 'url', 'remediationRecommendation_url' - A link to more information about the recommended remediation for this
-- vulnerability.
newRemediationRecommendation ::
  RemediationRecommendation
newRemediationRecommendation =
  RemediationRecommendation'
    { text = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The recommended course of action to remediate the finding.
remediationRecommendation_text :: Lens.Lens' RemediationRecommendation (Prelude.Maybe Prelude.Text)
remediationRecommendation_text = Lens.lens (\RemediationRecommendation' {text} -> text) (\s@RemediationRecommendation' {} a -> s {text = a} :: RemediationRecommendation)

-- | A link to more information about the recommended remediation for this
-- vulnerability.
remediationRecommendation_url :: Lens.Lens' RemediationRecommendation (Prelude.Maybe Prelude.Text)
remediationRecommendation_url = Lens.lens (\RemediationRecommendation' {url} -> url) (\s@RemediationRecommendation' {} a -> s {url = a} :: RemediationRecommendation)

instance Data.FromJSON RemediationRecommendation where
  parseJSON =
    Data.withObject
      "RemediationRecommendation"
      ( \x ->
          RemediationRecommendation'
            Prelude.<$> (x Data..:? "text")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable RemediationRecommendation where
  hashWithSalt _salt RemediationRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` url

instance Prelude.NFData RemediationRecommendation where
  rnf RemediationRecommendation' {..} =
    Prelude.rnf text `Prelude.seq` Prelude.rnf url
