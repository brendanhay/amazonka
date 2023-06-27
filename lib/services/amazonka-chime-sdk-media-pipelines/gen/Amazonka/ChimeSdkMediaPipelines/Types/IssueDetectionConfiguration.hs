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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.IssueDetectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.IssueDetectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for an issue
-- detection task.
--
-- /See:/ 'newIssueDetectionConfiguration' smart constructor.
data IssueDetectionConfiguration = IssueDetectionConfiguration'
  { -- | The name of the issue detection rule.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IssueDetectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'issueDetectionConfiguration_ruleName' - The name of the issue detection rule.
newIssueDetectionConfiguration ::
  -- | 'ruleName'
  Prelude.Text ->
  IssueDetectionConfiguration
newIssueDetectionConfiguration pRuleName_ =
  IssueDetectionConfiguration' {ruleName = pRuleName_}

-- | The name of the issue detection rule.
issueDetectionConfiguration_ruleName :: Lens.Lens' IssueDetectionConfiguration Prelude.Text
issueDetectionConfiguration_ruleName = Lens.lens (\IssueDetectionConfiguration' {ruleName} -> ruleName) (\s@IssueDetectionConfiguration' {} a -> s {ruleName = a} :: IssueDetectionConfiguration)

instance Data.FromJSON IssueDetectionConfiguration where
  parseJSON =
    Data.withObject
      "IssueDetectionConfiguration"
      ( \x ->
          IssueDetectionConfiguration'
            Prelude.<$> (x Data..: "RuleName")
      )

instance Prelude.Hashable IssueDetectionConfiguration where
  hashWithSalt _salt IssueDetectionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` ruleName

instance Prelude.NFData IssueDetectionConfiguration where
  rnf IssueDetectionConfiguration' {..} =
    Prelude.rnf ruleName

instance Data.ToJSON IssueDetectionConfiguration where
  toJSON IssueDetectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RuleName" Data..= ruleName)]
      )
