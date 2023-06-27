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
-- Module      : Amazonka.Kendra.Types.SaaSConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SaaSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to GitHub Enterprise
-- Cloud (SaaS).
--
-- /See:/ 'newSaaSConfiguration' smart constructor.
data SaaSConfiguration = SaaSConfiguration'
  { -- | The name of the organization of the GitHub Enterprise Cloud (SaaS)
    -- account you want to connect to. You can find your organization name by
    -- logging into GitHub desktop and selecting __Your organizations__ under
    -- your profile picture dropdown.
    organizationName :: Prelude.Text,
    -- | The GitHub host URL or API endpoint URL. For example,
    -- /https:\/\/api.github.com/.
    hostUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SaaSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationName', 'saaSConfiguration_organizationName' - The name of the organization of the GitHub Enterprise Cloud (SaaS)
-- account you want to connect to. You can find your organization name by
-- logging into GitHub desktop and selecting __Your organizations__ under
-- your profile picture dropdown.
--
-- 'hostUrl', 'saaSConfiguration_hostUrl' - The GitHub host URL or API endpoint URL. For example,
-- /https:\/\/api.github.com/.
newSaaSConfiguration ::
  -- | 'organizationName'
  Prelude.Text ->
  -- | 'hostUrl'
  Prelude.Text ->
  SaaSConfiguration
newSaaSConfiguration pOrganizationName_ pHostUrl_ =
  SaaSConfiguration'
    { organizationName =
        pOrganizationName_,
      hostUrl = pHostUrl_
    }

-- | The name of the organization of the GitHub Enterprise Cloud (SaaS)
-- account you want to connect to. You can find your organization name by
-- logging into GitHub desktop and selecting __Your organizations__ under
-- your profile picture dropdown.
saaSConfiguration_organizationName :: Lens.Lens' SaaSConfiguration Prelude.Text
saaSConfiguration_organizationName = Lens.lens (\SaaSConfiguration' {organizationName} -> organizationName) (\s@SaaSConfiguration' {} a -> s {organizationName = a} :: SaaSConfiguration)

-- | The GitHub host URL or API endpoint URL. For example,
-- /https:\/\/api.github.com/.
saaSConfiguration_hostUrl :: Lens.Lens' SaaSConfiguration Prelude.Text
saaSConfiguration_hostUrl = Lens.lens (\SaaSConfiguration' {hostUrl} -> hostUrl) (\s@SaaSConfiguration' {} a -> s {hostUrl = a} :: SaaSConfiguration)

instance Data.FromJSON SaaSConfiguration where
  parseJSON =
    Data.withObject
      "SaaSConfiguration"
      ( \x ->
          SaaSConfiguration'
            Prelude.<$> (x Data..: "OrganizationName")
            Prelude.<*> (x Data..: "HostUrl")
      )

instance Prelude.Hashable SaaSConfiguration where
  hashWithSalt _salt SaaSConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` organizationName
      `Prelude.hashWithSalt` hostUrl

instance Prelude.NFData SaaSConfiguration where
  rnf SaaSConfiguration' {..} =
    Prelude.rnf organizationName
      `Prelude.seq` Prelude.rnf hostUrl

instance Data.ToJSON SaaSConfiguration where
  toJSON SaaSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationName" Data..= organizationName),
            Prelude.Just ("HostUrl" Data..= hostUrl)
          ]
      )
