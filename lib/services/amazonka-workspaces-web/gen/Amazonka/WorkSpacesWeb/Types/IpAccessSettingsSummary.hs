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
-- Module      : Amazonka.WorkSpacesWeb.Types.IpAccessSettingsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IpAccessSettingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of IP access settings.
--
-- /See:/ 'newIpAccessSettingsSummary' smart constructor.
data IpAccessSettingsSummary = IpAccessSettingsSummary'
  { -- | The creation date timestamp of the IP access settings.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the IP access settings.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The display name of the IP access settings.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of IP access settings.
    ipAccessSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAccessSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'ipAccessSettingsSummary_creationDate' - The creation date timestamp of the IP access settings.
--
-- 'description', 'ipAccessSettingsSummary_description' - The description of the IP access settings.
--
-- 'displayName', 'ipAccessSettingsSummary_displayName' - The display name of the IP access settings.
--
-- 'ipAccessSettingsArn', 'ipAccessSettingsSummary_ipAccessSettingsArn' - The ARN of IP access settings.
newIpAccessSettingsSummary ::
  IpAccessSettingsSummary
newIpAccessSettingsSummary =
  IpAccessSettingsSummary'
    { creationDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      ipAccessSettingsArn = Prelude.Nothing
    }

-- | The creation date timestamp of the IP access settings.
ipAccessSettingsSummary_creationDate :: Lens.Lens' IpAccessSettingsSummary (Prelude.Maybe Prelude.UTCTime)
ipAccessSettingsSummary_creationDate = Lens.lens (\IpAccessSettingsSummary' {creationDate} -> creationDate) (\s@IpAccessSettingsSummary' {} a -> s {creationDate = a} :: IpAccessSettingsSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the IP access settings.
ipAccessSettingsSummary_description :: Lens.Lens' IpAccessSettingsSummary (Prelude.Maybe Prelude.Text)
ipAccessSettingsSummary_description = Lens.lens (\IpAccessSettingsSummary' {description} -> description) (\s@IpAccessSettingsSummary' {} a -> s {description = a} :: IpAccessSettingsSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The display name of the IP access settings.
ipAccessSettingsSummary_displayName :: Lens.Lens' IpAccessSettingsSummary (Prelude.Maybe Prelude.Text)
ipAccessSettingsSummary_displayName = Lens.lens (\IpAccessSettingsSummary' {displayName} -> displayName) (\s@IpAccessSettingsSummary' {} a -> s {displayName = a} :: IpAccessSettingsSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of IP access settings.
ipAccessSettingsSummary_ipAccessSettingsArn :: Lens.Lens' IpAccessSettingsSummary (Prelude.Maybe Prelude.Text)
ipAccessSettingsSummary_ipAccessSettingsArn = Lens.lens (\IpAccessSettingsSummary' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@IpAccessSettingsSummary' {} a -> s {ipAccessSettingsArn = a} :: IpAccessSettingsSummary)

instance Data.FromJSON IpAccessSettingsSummary where
  parseJSON =
    Data.withObject
      "IpAccessSettingsSummary"
      ( \x ->
          IpAccessSettingsSummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "ipAccessSettingsArn")
      )

instance Prelude.Hashable IpAccessSettingsSummary where
  hashWithSalt _salt IpAccessSettingsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` ipAccessSettingsArn

instance Prelude.NFData IpAccessSettingsSummary where
  rnf IpAccessSettingsSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf ipAccessSettingsArn
