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
-- Module      : Amazonka.AppConfig.Types.HostedConfigurationVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.HostedConfigurationVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the configuration.
--
-- /See:/ 'newHostedConfigurationVersionSummary' smart constructor.
data HostedConfigurationVersionSummary = HostedConfigurationVersionSummary'
  { -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the configuration content.
    -- For more information, see
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | A description of the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A user-defined label for an AppConfig hosted configuration version.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | The configuration version.
    versionNumber :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostedConfigurationVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'hostedConfigurationVersionSummary_applicationId' - The application ID.
--
-- 'configurationProfileId', 'hostedConfigurationVersionSummary_configurationProfileId' - The configuration profile ID.
--
-- 'contentType', 'hostedConfigurationVersionSummary_contentType' - A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
--
-- 'description', 'hostedConfigurationVersionSummary_description' - A description of the configuration.
--
-- 'versionLabel', 'hostedConfigurationVersionSummary_versionLabel' - A user-defined label for an AppConfig hosted configuration version.
--
-- 'versionNumber', 'hostedConfigurationVersionSummary_versionNumber' - The configuration version.
newHostedConfigurationVersionSummary ::
  HostedConfigurationVersionSummary
newHostedConfigurationVersionSummary =
  HostedConfigurationVersionSummary'
    { applicationId =
        Prelude.Nothing,
      configurationProfileId = Prelude.Nothing,
      contentType = Prelude.Nothing,
      description = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The application ID.
hostedConfigurationVersionSummary_applicationId :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Text)
hostedConfigurationVersionSummary_applicationId = Lens.lens (\HostedConfigurationVersionSummary' {applicationId} -> applicationId) (\s@HostedConfigurationVersionSummary' {} a -> s {applicationId = a} :: HostedConfigurationVersionSummary)

-- | The configuration profile ID.
hostedConfigurationVersionSummary_configurationProfileId :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Text)
hostedConfigurationVersionSummary_configurationProfileId = Lens.lens (\HostedConfigurationVersionSummary' {configurationProfileId} -> configurationProfileId) (\s@HostedConfigurationVersionSummary' {} a -> s {configurationProfileId = a} :: HostedConfigurationVersionSummary)

-- | A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
hostedConfigurationVersionSummary_contentType :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Text)
hostedConfigurationVersionSummary_contentType = Lens.lens (\HostedConfigurationVersionSummary' {contentType} -> contentType) (\s@HostedConfigurationVersionSummary' {} a -> s {contentType = a} :: HostedConfigurationVersionSummary)

-- | A description of the configuration.
hostedConfigurationVersionSummary_description :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Text)
hostedConfigurationVersionSummary_description = Lens.lens (\HostedConfigurationVersionSummary' {description} -> description) (\s@HostedConfigurationVersionSummary' {} a -> s {description = a} :: HostedConfigurationVersionSummary)

-- | A user-defined label for an AppConfig hosted configuration version.
hostedConfigurationVersionSummary_versionLabel :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Text)
hostedConfigurationVersionSummary_versionLabel = Lens.lens (\HostedConfigurationVersionSummary' {versionLabel} -> versionLabel) (\s@HostedConfigurationVersionSummary' {} a -> s {versionLabel = a} :: HostedConfigurationVersionSummary)

-- | The configuration version.
hostedConfigurationVersionSummary_versionNumber :: Lens.Lens' HostedConfigurationVersionSummary (Prelude.Maybe Prelude.Int)
hostedConfigurationVersionSummary_versionNumber = Lens.lens (\HostedConfigurationVersionSummary' {versionNumber} -> versionNumber) (\s@HostedConfigurationVersionSummary' {} a -> s {versionNumber = a} :: HostedConfigurationVersionSummary)

instance
  Data.FromJSON
    HostedConfigurationVersionSummary
  where
  parseJSON =
    Data.withObject
      "HostedConfigurationVersionSummary"
      ( \x ->
          HostedConfigurationVersionSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "ConfigurationProfileId")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "VersionLabel")
            Prelude.<*> (x Data..:? "VersionNumber")
      )

instance
  Prelude.Hashable
    HostedConfigurationVersionSummary
  where
  hashWithSalt
    _salt
    HostedConfigurationVersionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` applicationId
        `Prelude.hashWithSalt` configurationProfileId
        `Prelude.hashWithSalt` contentType
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` versionLabel
        `Prelude.hashWithSalt` versionNumber

instance
  Prelude.NFData
    HostedConfigurationVersionSummary
  where
  rnf HostedConfigurationVersionSummary' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionLabel
      `Prelude.seq` Prelude.rnf versionNumber
