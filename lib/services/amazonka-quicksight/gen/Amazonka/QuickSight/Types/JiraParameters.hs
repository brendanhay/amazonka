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
-- Module      : Amazonka.QuickSight.Types.JiraParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.JiraParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Jira.
--
-- /See:/ 'newJiraParameters' smart constructor.
data JiraParameters = JiraParameters'
  { -- | The base URL of the Jira site.
    siteBaseUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JiraParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteBaseUrl', 'jiraParameters_siteBaseUrl' - The base URL of the Jira site.
newJiraParameters ::
  -- | 'siteBaseUrl'
  Prelude.Text ->
  JiraParameters
newJiraParameters pSiteBaseUrl_ =
  JiraParameters' {siteBaseUrl = pSiteBaseUrl_}

-- | The base URL of the Jira site.
jiraParameters_siteBaseUrl :: Lens.Lens' JiraParameters Prelude.Text
jiraParameters_siteBaseUrl = Lens.lens (\JiraParameters' {siteBaseUrl} -> siteBaseUrl) (\s@JiraParameters' {} a -> s {siteBaseUrl = a} :: JiraParameters)

instance Data.FromJSON JiraParameters where
  parseJSON =
    Data.withObject
      "JiraParameters"
      ( \x ->
          JiraParameters'
            Prelude.<$> (x Data..: "SiteBaseUrl")
      )

instance Prelude.Hashable JiraParameters where
  hashWithSalt _salt JiraParameters' {..} =
    _salt `Prelude.hashWithSalt` siteBaseUrl

instance Prelude.NFData JiraParameters where
  rnf JiraParameters' {..} = Prelude.rnf siteBaseUrl

instance Data.ToJSON JiraParameters where
  toJSON JiraParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SiteBaseUrl" Data..= siteBaseUrl)]
      )
