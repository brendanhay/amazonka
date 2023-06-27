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
-- Module      : Amazonka.WorkSpacesWeb.Types.IpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IpAccessSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.IpRule

-- | The IP access settings resource that can be associated with a web
-- portal.
--
-- /See:/ 'newIpAccessSettings' smart constructor.
data IpAccessSettings = IpAccessSettings'
  { -- | A list of web portal ARNs that this IP access settings resource is
    -- associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text],
    -- | The creation date timestamp of the IP access settings.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the IP access settings.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The display name of the IP access settings.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The IP rules of the IP access settings.
    ipRules :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty IpRule)),
    -- | The ARN of the IP access settings resource.
    ipAccessSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedPortalArns', 'ipAccessSettings_associatedPortalArns' - A list of web portal ARNs that this IP access settings resource is
-- associated with.
--
-- 'creationDate', 'ipAccessSettings_creationDate' - The creation date timestamp of the IP access settings.
--
-- 'description', 'ipAccessSettings_description' - The description of the IP access settings.
--
-- 'displayName', 'ipAccessSettings_displayName' - The display name of the IP access settings.
--
-- 'ipRules', 'ipAccessSettings_ipRules' - The IP rules of the IP access settings.
--
-- 'ipAccessSettingsArn', 'ipAccessSettings_ipAccessSettingsArn' - The ARN of the IP access settings resource.
newIpAccessSettings ::
  -- | 'ipAccessSettingsArn'
  Prelude.Text ->
  IpAccessSettings
newIpAccessSettings pIpAccessSettingsArn_ =
  IpAccessSettings'
    { associatedPortalArns =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      ipRules = Prelude.Nothing,
      ipAccessSettingsArn = pIpAccessSettingsArn_
    }

-- | A list of web portal ARNs that this IP access settings resource is
-- associated with.
ipAccessSettings_associatedPortalArns :: Lens.Lens' IpAccessSettings (Prelude.Maybe [Prelude.Text])
ipAccessSettings_associatedPortalArns = Lens.lens (\IpAccessSettings' {associatedPortalArns} -> associatedPortalArns) (\s@IpAccessSettings' {} a -> s {associatedPortalArns = a} :: IpAccessSettings) Prelude.. Lens.mapping Lens.coerced

-- | The creation date timestamp of the IP access settings.
ipAccessSettings_creationDate :: Lens.Lens' IpAccessSettings (Prelude.Maybe Prelude.UTCTime)
ipAccessSettings_creationDate = Lens.lens (\IpAccessSettings' {creationDate} -> creationDate) (\s@IpAccessSettings' {} a -> s {creationDate = a} :: IpAccessSettings) Prelude.. Lens.mapping Data._Time

-- | The description of the IP access settings.
ipAccessSettings_description :: Lens.Lens' IpAccessSettings (Prelude.Maybe Prelude.Text)
ipAccessSettings_description = Lens.lens (\IpAccessSettings' {description} -> description) (\s@IpAccessSettings' {} a -> s {description = a} :: IpAccessSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The display name of the IP access settings.
ipAccessSettings_displayName :: Lens.Lens' IpAccessSettings (Prelude.Maybe Prelude.Text)
ipAccessSettings_displayName = Lens.lens (\IpAccessSettings' {displayName} -> displayName) (\s@IpAccessSettings' {} a -> s {displayName = a} :: IpAccessSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The IP rules of the IP access settings.
ipAccessSettings_ipRules :: Lens.Lens' IpAccessSettings (Prelude.Maybe (Prelude.NonEmpty IpRule))
ipAccessSettings_ipRules = Lens.lens (\IpAccessSettings' {ipRules} -> ipRules) (\s@IpAccessSettings' {} a -> s {ipRules = a} :: IpAccessSettings) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ARN of the IP access settings resource.
ipAccessSettings_ipAccessSettingsArn :: Lens.Lens' IpAccessSettings Prelude.Text
ipAccessSettings_ipAccessSettingsArn = Lens.lens (\IpAccessSettings' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@IpAccessSettings' {} a -> s {ipAccessSettingsArn = a} :: IpAccessSettings)

instance Data.FromJSON IpAccessSettings where
  parseJSON =
    Data.withObject
      "IpAccessSettings"
      ( \x ->
          IpAccessSettings'
            Prelude.<$> ( x
                            Data..:? "associatedPortalArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "ipRules")
            Prelude.<*> (x Data..: "ipAccessSettingsArn")
      )

instance Prelude.Hashable IpAccessSettings where
  hashWithSalt _salt IpAccessSettings' {..} =
    _salt
      `Prelude.hashWithSalt` associatedPortalArns
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` ipRules
      `Prelude.hashWithSalt` ipAccessSettingsArn

instance Prelude.NFData IpAccessSettings where
  rnf IpAccessSettings' {..} =
    Prelude.rnf associatedPortalArns
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf ipRules
      `Prelude.seq` Prelude.rnf ipAccessSettingsArn
