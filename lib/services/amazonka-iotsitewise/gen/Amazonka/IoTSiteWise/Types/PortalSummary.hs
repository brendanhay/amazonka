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
-- Module      : Amazonka.IoTSiteWise.Types.PortalSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PortalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PortalStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a portal summary.
--
-- /See:/ 'newPortalSummary' smart constructor.
data PortalSummary = PortalSummary'
  { -- | The date the portal was created, in Unix epoch time.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The portal\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date the portal was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Data.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the service role that allows the portal\'s users to access your IoT
    -- SiteWise resources on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
    -- in the /IoT SiteWise User Guide/.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the portal.
    id :: Prelude.Text,
    -- | The name of the portal.
    name :: Prelude.Text,
    -- | The URL for the IoT SiteWise Monitor portal. You can use this URL to
    -- access portals that use IAM Identity Center for authentication. For
    -- portals that use IAM for authentication, you must use the IoT SiteWise
    -- console to get a URL that you can use to access the portal.
    startUrl :: Prelude.Text,
    status :: PortalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'portalSummary_creationDate' - The date the portal was created, in Unix epoch time.
--
-- 'description', 'portalSummary_description' - The portal\'s description.
--
-- 'lastUpdateDate', 'portalSummary_lastUpdateDate' - The date the portal was last updated, in Unix epoch time.
--
-- 'roleArn', 'portalSummary_roleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
--
-- 'id', 'portalSummary_id' - The ID of the portal.
--
-- 'name', 'portalSummary_name' - The name of the portal.
--
-- 'startUrl', 'portalSummary_startUrl' - The URL for the IoT SiteWise Monitor portal. You can use this URL to
-- access portals that use IAM Identity Center for authentication. For
-- portals that use IAM for authentication, you must use the IoT SiteWise
-- console to get a URL that you can use to access the portal.
--
-- 'status', 'portalSummary_status' - Undocumented member.
newPortalSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'startUrl'
  Prelude.Text ->
  -- | 'status'
  PortalStatus ->
  PortalSummary
newPortalSummary pId_ pName_ pStartUrl_ pStatus_ =
  PortalSummary'
    { creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdateDate = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      id = pId_,
      name = pName_,
      startUrl = pStartUrl_,
      status = pStatus_
    }

-- | The date the portal was created, in Unix epoch time.
portalSummary_creationDate :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.UTCTime)
portalSummary_creationDate = Lens.lens (\PortalSummary' {creationDate} -> creationDate) (\s@PortalSummary' {} a -> s {creationDate = a} :: PortalSummary) Prelude.. Lens.mapping Data._Time

-- | The portal\'s description.
portalSummary_description :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_description = Lens.lens (\PortalSummary' {description} -> description) (\s@PortalSummary' {} a -> s {description = a} :: PortalSummary)

-- | The date the portal was last updated, in Unix epoch time.
portalSummary_lastUpdateDate :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.UTCTime)
portalSummary_lastUpdateDate = Lens.lens (\PortalSummary' {lastUpdateDate} -> lastUpdateDate) (\s@PortalSummary' {} a -> s {lastUpdateDate = a} :: PortalSummary) Prelude.. Lens.mapping Data._Time

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
portalSummary_roleArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_roleArn = Lens.lens (\PortalSummary' {roleArn} -> roleArn) (\s@PortalSummary' {} a -> s {roleArn = a} :: PortalSummary)

-- | The ID of the portal.
portalSummary_id :: Lens.Lens' PortalSummary Prelude.Text
portalSummary_id = Lens.lens (\PortalSummary' {id} -> id) (\s@PortalSummary' {} a -> s {id = a} :: PortalSummary)

-- | The name of the portal.
portalSummary_name :: Lens.Lens' PortalSummary Prelude.Text
portalSummary_name = Lens.lens (\PortalSummary' {name} -> name) (\s@PortalSummary' {} a -> s {name = a} :: PortalSummary)

-- | The URL for the IoT SiteWise Monitor portal. You can use this URL to
-- access portals that use IAM Identity Center for authentication. For
-- portals that use IAM for authentication, you must use the IoT SiteWise
-- console to get a URL that you can use to access the portal.
portalSummary_startUrl :: Lens.Lens' PortalSummary Prelude.Text
portalSummary_startUrl = Lens.lens (\PortalSummary' {startUrl} -> startUrl) (\s@PortalSummary' {} a -> s {startUrl = a} :: PortalSummary)

-- | Undocumented member.
portalSummary_status :: Lens.Lens' PortalSummary PortalStatus
portalSummary_status = Lens.lens (\PortalSummary' {status} -> status) (\s@PortalSummary' {} a -> s {status = a} :: PortalSummary)

instance Data.FromJSON PortalSummary where
  parseJSON =
    Data.withObject
      "PortalSummary"
      ( \x ->
          PortalSummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdateDate")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "startUrl")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable PortalSummary where
  hashWithSalt _salt PortalSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startUrl
      `Prelude.hashWithSalt` status

instance Prelude.NFData PortalSummary where
  rnf PortalSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startUrl
      `Prelude.seq` Prelude.rnf status
