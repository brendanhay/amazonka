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
-- Module      : Amazonka.DirectoryService.Types.DirectoryDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.DirectoryConnectSettingsDescription
import Amazonka.DirectoryService.Types.DirectoryEdition
import Amazonka.DirectoryService.Types.DirectorySize
import Amazonka.DirectoryService.Types.DirectoryStage
import Amazonka.DirectoryService.Types.DirectoryType
import Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription
import Amazonka.DirectoryService.Types.OSVersion
import Amazonka.DirectoryService.Types.OwnerDirectoryDescription
import Amazonka.DirectoryService.Types.RadiusSettings
import Amazonka.DirectoryService.Types.RadiusStatus
import Amazonka.DirectoryService.Types.RegionsInfo
import Amazonka.DirectoryService.Types.ShareMethod
import Amazonka.DirectoryService.Types.ShareStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Directory Service directory.
--
-- /See:/ 'newDirectoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
  { -- | The access URL for the directory, such as
    -- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
    -- directory, @\<alias>@ is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    accessUrl :: Prelude.Maybe Prelude.Text,
    -- | The alias for the directory. If no alias has been created for the
    -- directory, the alias is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A DirectoryConnectSettingsDescription object that contains additional
    -- information about an AD Connector directory. This member is only present
    -- if the directory is an AD Connector directory.
    connectSettings :: Prelude.Maybe DirectoryConnectSettingsDescription,
    -- | The description for the directory.
    description :: Prelude.Maybe Prelude.Text,
    -- | The desired number of domain controllers in the directory if the
    -- directory is Microsoft AD.
    desiredNumberOfDomainControllers :: Prelude.Maybe Prelude.Natural,
    -- | The directory identifier.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The IP addresses of the DNS servers for the directory. For a Simple AD
    -- or Microsoft AD directory, these are the IP addresses of the Simple AD
    -- or Microsoft AD directory servers. For an AD Connector directory, these
    -- are the IP addresses of the DNS servers or domain controllers in your
    -- self-managed directory to which the AD Connector is connected.
    dnsIpAddrs :: Prelude.Maybe [Prelude.Text],
    -- | The edition associated with this directory.
    edition :: Prelude.Maybe DirectoryEdition,
    -- | Specifies when the directory was created.
    launchTime :: Prelude.Maybe Data.POSIX,
    -- | The fully qualified name of the directory.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system (OS) version of the directory.
    osVersion :: Prelude.Maybe OSVersion,
    -- | Describes the Managed Microsoft AD directory in the directory owner
    -- account.
    ownerDirectoryDescription :: Prelude.Maybe OwnerDirectoryDescription,
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server configured for this directory.
    radiusSettings :: Prelude.Maybe RadiusSettings,
    -- | The status of the RADIUS MFA server connection.
    radiusStatus :: Prelude.Maybe RadiusStatus,
    -- | Lists the Regions where the directory has replicated.
    regionsInfo :: Prelude.Maybe RegionsInfo,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your Amazon Web Services organization
    -- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
    -- shared directory request (@HANDSHAKE@).
    shareMethod :: Prelude.Maybe ShareMethod,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Current directory status of the shared Managed Microsoft AD directory.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | The short name of the directory.
    shortName :: Prelude.Maybe Prelude.Text,
    -- | The directory size.
    size :: Prelude.Maybe DirectorySize,
    -- | Indicates if single sign-on is enabled for the directory. For more
    -- information, see EnableSso and DisableSso.
    ssoEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The current stage of the directory.
    stage :: Prelude.Maybe DirectoryStage,
    -- | The date and time that the stage was last updated.
    stageLastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Additional information about the directory stage.
    stageReason :: Prelude.Maybe Prelude.Text,
    -- | The directory size.
    type' :: Prelude.Maybe DirectoryType,
    -- | A DirectoryVpcSettingsDescription object that contains additional
    -- information about a directory. This member is only present if the
    -- directory is a Simple AD or Managed Microsoft AD directory.
    vpcSettings :: Prelude.Maybe DirectoryVpcSettingsDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessUrl', 'directoryDescription_accessUrl' - The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'alias', 'directoryDescription_alias' - The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'connectSettings', 'directoryDescription_connectSettings' - A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
--
-- 'description', 'directoryDescription_description' - The description for the directory.
--
-- 'desiredNumberOfDomainControllers', 'directoryDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
--
-- 'directoryId', 'directoryDescription_directoryId' - The directory identifier.
--
-- 'dnsIpAddrs', 'directoryDescription_dnsIpAddrs' - The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in your
-- self-managed directory to which the AD Connector is connected.
--
-- 'edition', 'directoryDescription_edition' - The edition associated with this directory.
--
-- 'launchTime', 'directoryDescription_launchTime' - Specifies when the directory was created.
--
-- 'name', 'directoryDescription_name' - The fully qualified name of the directory.
--
-- 'osVersion', 'directoryDescription_osVersion' - The operating system (OS) version of the directory.
--
-- 'ownerDirectoryDescription', 'directoryDescription_ownerDirectoryDescription' - Describes the Managed Microsoft AD directory in the directory owner
-- account.
--
-- 'radiusSettings', 'directoryDescription_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
--
-- 'radiusStatus', 'directoryDescription_radiusStatus' - The status of the RADIUS MFA server connection.
--
-- 'regionsInfo', 'directoryDescription_regionsInfo' - Lists the Regions where the directory has replicated.
--
-- 'shareMethod', 'directoryDescription_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
--
-- 'shareNotes', 'directoryDescription_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'shareStatus', 'directoryDescription_shareStatus' - Current directory status of the shared Managed Microsoft AD directory.
--
-- 'shortName', 'directoryDescription_shortName' - The short name of the directory.
--
-- 'size', 'directoryDescription_size' - The directory size.
--
-- 'ssoEnabled', 'directoryDescription_ssoEnabled' - Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
--
-- 'stage', 'directoryDescription_stage' - The current stage of the directory.
--
-- 'stageLastUpdatedDateTime', 'directoryDescription_stageLastUpdatedDateTime' - The date and time that the stage was last updated.
--
-- 'stageReason', 'directoryDescription_stageReason' - Additional information about the directory stage.
--
-- 'type'', 'directoryDescription_type' - The directory size.
--
-- 'vpcSettings', 'directoryDescription_vpcSettings' - A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed Microsoft AD directory.
newDirectoryDescription ::
  DirectoryDescription
newDirectoryDescription =
  DirectoryDescription'
    { accessUrl = Prelude.Nothing,
      alias = Prelude.Nothing,
      connectSettings = Prelude.Nothing,
      description = Prelude.Nothing,
      desiredNumberOfDomainControllers = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      dnsIpAddrs = Prelude.Nothing,
      edition = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      name = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      ownerDirectoryDescription = Prelude.Nothing,
      radiusSettings = Prelude.Nothing,
      radiusStatus = Prelude.Nothing,
      regionsInfo = Prelude.Nothing,
      shareMethod = Prelude.Nothing,
      shareNotes = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      shortName = Prelude.Nothing,
      size = Prelude.Nothing,
      ssoEnabled = Prelude.Nothing,
      stage = Prelude.Nothing,
      stageLastUpdatedDateTime = Prelude.Nothing,
      stageReason = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcSettings = Prelude.Nothing
    }

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_accessUrl :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_accessUrl = Lens.lens (\DirectoryDescription' {accessUrl} -> accessUrl) (\s@DirectoryDescription' {} a -> s {accessUrl = a} :: DirectoryDescription)

-- | The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_alias :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_alias = Lens.lens (\DirectoryDescription' {alias} -> alias) (\s@DirectoryDescription' {} a -> s {alias = a} :: DirectoryDescription)

-- | A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
directoryDescription_connectSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryConnectSettingsDescription)
directoryDescription_connectSettings = Lens.lens (\DirectoryDescription' {connectSettings} -> connectSettings) (\s@DirectoryDescription' {} a -> s {connectSettings = a} :: DirectoryDescription)

-- | The description for the directory.
directoryDescription_description :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_description = Lens.lens (\DirectoryDescription' {description} -> description) (\s@DirectoryDescription' {} a -> s {description = a} :: DirectoryDescription)

-- | The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
directoryDescription_desiredNumberOfDomainControllers :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Natural)
directoryDescription_desiredNumberOfDomainControllers = Lens.lens (\DirectoryDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@DirectoryDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: DirectoryDescription)

-- | The directory identifier.
directoryDescription_directoryId :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_directoryId = Lens.lens (\DirectoryDescription' {directoryId} -> directoryId) (\s@DirectoryDescription' {} a -> s {directoryId = a} :: DirectoryDescription)

-- | The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in your
-- self-managed directory to which the AD Connector is connected.
directoryDescription_dnsIpAddrs :: Lens.Lens' DirectoryDescription (Prelude.Maybe [Prelude.Text])
directoryDescription_dnsIpAddrs = Lens.lens (\DirectoryDescription' {dnsIpAddrs} -> dnsIpAddrs) (\s@DirectoryDescription' {} a -> s {dnsIpAddrs = a} :: DirectoryDescription) Prelude.. Lens.mapping Lens.coerced

-- | The edition associated with this directory.
directoryDescription_edition :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryEdition)
directoryDescription_edition = Lens.lens (\DirectoryDescription' {edition} -> edition) (\s@DirectoryDescription' {} a -> s {edition = a} :: DirectoryDescription)

-- | Specifies when the directory was created.
directoryDescription_launchTime :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.UTCTime)
directoryDescription_launchTime = Lens.lens (\DirectoryDescription' {launchTime} -> launchTime) (\s@DirectoryDescription' {} a -> s {launchTime = a} :: DirectoryDescription) Prelude.. Lens.mapping Data._Time

-- | The fully qualified name of the directory.
directoryDescription_name :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_name = Lens.lens (\DirectoryDescription' {name} -> name) (\s@DirectoryDescription' {} a -> s {name = a} :: DirectoryDescription)

-- | The operating system (OS) version of the directory.
directoryDescription_osVersion :: Lens.Lens' DirectoryDescription (Prelude.Maybe OSVersion)
directoryDescription_osVersion = Lens.lens (\DirectoryDescription' {osVersion} -> osVersion) (\s@DirectoryDescription' {} a -> s {osVersion = a} :: DirectoryDescription)

-- | Describes the Managed Microsoft AD directory in the directory owner
-- account.
directoryDescription_ownerDirectoryDescription :: Lens.Lens' DirectoryDescription (Prelude.Maybe OwnerDirectoryDescription)
directoryDescription_ownerDirectoryDescription = Lens.lens (\DirectoryDescription' {ownerDirectoryDescription} -> ownerDirectoryDescription) (\s@DirectoryDescription' {} a -> s {ownerDirectoryDescription = a} :: DirectoryDescription)

-- | A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
directoryDescription_radiusSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe RadiusSettings)
directoryDescription_radiusSettings = Lens.lens (\DirectoryDescription' {radiusSettings} -> radiusSettings) (\s@DirectoryDescription' {} a -> s {radiusSettings = a} :: DirectoryDescription)

-- | The status of the RADIUS MFA server connection.
directoryDescription_radiusStatus :: Lens.Lens' DirectoryDescription (Prelude.Maybe RadiusStatus)
directoryDescription_radiusStatus = Lens.lens (\DirectoryDescription' {radiusStatus} -> radiusStatus) (\s@DirectoryDescription' {} a -> s {radiusStatus = a} :: DirectoryDescription)

-- | Lists the Regions where the directory has replicated.
directoryDescription_regionsInfo :: Lens.Lens' DirectoryDescription (Prelude.Maybe RegionsInfo)
directoryDescription_regionsInfo = Lens.lens (\DirectoryDescription' {regionsInfo} -> regionsInfo) (\s@DirectoryDescription' {} a -> s {regionsInfo = a} :: DirectoryDescription)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
directoryDescription_shareMethod :: Lens.Lens' DirectoryDescription (Prelude.Maybe ShareMethod)
directoryDescription_shareMethod = Lens.lens (\DirectoryDescription' {shareMethod} -> shareMethod) (\s@DirectoryDescription' {} a -> s {shareMethod = a} :: DirectoryDescription)

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
directoryDescription_shareNotes :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_shareNotes = Lens.lens (\DirectoryDescription' {shareNotes} -> shareNotes) (\s@DirectoryDescription' {} a -> s {shareNotes = a} :: DirectoryDescription) Prelude.. Lens.mapping Data._Sensitive

-- | Current directory status of the shared Managed Microsoft AD directory.
directoryDescription_shareStatus :: Lens.Lens' DirectoryDescription (Prelude.Maybe ShareStatus)
directoryDescription_shareStatus = Lens.lens (\DirectoryDescription' {shareStatus} -> shareStatus) (\s@DirectoryDescription' {} a -> s {shareStatus = a} :: DirectoryDescription)

-- | The short name of the directory.
directoryDescription_shortName :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_shortName = Lens.lens (\DirectoryDescription' {shortName} -> shortName) (\s@DirectoryDescription' {} a -> s {shortName = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_size :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectorySize)
directoryDescription_size = Lens.lens (\DirectoryDescription' {size} -> size) (\s@DirectoryDescription' {} a -> s {size = a} :: DirectoryDescription)

-- | Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
directoryDescription_ssoEnabled :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Bool)
directoryDescription_ssoEnabled = Lens.lens (\DirectoryDescription' {ssoEnabled} -> ssoEnabled) (\s@DirectoryDescription' {} a -> s {ssoEnabled = a} :: DirectoryDescription)

-- | The current stage of the directory.
directoryDescription_stage :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryStage)
directoryDescription_stage = Lens.lens (\DirectoryDescription' {stage} -> stage) (\s@DirectoryDescription' {} a -> s {stage = a} :: DirectoryDescription)

-- | The date and time that the stage was last updated.
directoryDescription_stageLastUpdatedDateTime :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.UTCTime)
directoryDescription_stageLastUpdatedDateTime = Lens.lens (\DirectoryDescription' {stageLastUpdatedDateTime} -> stageLastUpdatedDateTime) (\s@DirectoryDescription' {} a -> s {stageLastUpdatedDateTime = a} :: DirectoryDescription) Prelude.. Lens.mapping Data._Time

-- | Additional information about the directory stage.
directoryDescription_stageReason :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_stageReason = Lens.lens (\DirectoryDescription' {stageReason} -> stageReason) (\s@DirectoryDescription' {} a -> s {stageReason = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_type :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryType)
directoryDescription_type = Lens.lens (\DirectoryDescription' {type'} -> type') (\s@DirectoryDescription' {} a -> s {type' = a} :: DirectoryDescription)

-- | A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed Microsoft AD directory.
directoryDescription_vpcSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryVpcSettingsDescription)
directoryDescription_vpcSettings = Lens.lens (\DirectoryDescription' {vpcSettings} -> vpcSettings) (\s@DirectoryDescription' {} a -> s {vpcSettings = a} :: DirectoryDescription)

instance Data.FromJSON DirectoryDescription where
  parseJSON =
    Data.withObject
      "DirectoryDescription"
      ( \x ->
          DirectoryDescription'
            Prelude.<$> (x Data..:? "AccessUrl")
            Prelude.<*> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "ConnectSettings")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DesiredNumberOfDomainControllers")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "DnsIpAddrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "LaunchTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OsVersion")
            Prelude.<*> (x Data..:? "OwnerDirectoryDescription")
            Prelude.<*> (x Data..:? "RadiusSettings")
            Prelude.<*> (x Data..:? "RadiusStatus")
            Prelude.<*> (x Data..:? "RegionsInfo")
            Prelude.<*> (x Data..:? "ShareMethod")
            Prelude.<*> (x Data..:? "ShareNotes")
            Prelude.<*> (x Data..:? "ShareStatus")
            Prelude.<*> (x Data..:? "ShortName")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "SsoEnabled")
            Prelude.<*> (x Data..:? "Stage")
            Prelude.<*> (x Data..:? "StageLastUpdatedDateTime")
            Prelude.<*> (x Data..:? "StageReason")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VpcSettings")
      )

instance Prelude.Hashable DirectoryDescription where
  hashWithSalt _salt DirectoryDescription' {..} =
    _salt `Prelude.hashWithSalt` accessUrl
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` connectSettings
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` desiredNumberOfDomainControllers
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` dnsIpAddrs
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` ownerDirectoryDescription
      `Prelude.hashWithSalt` radiusSettings
      `Prelude.hashWithSalt` radiusStatus
      `Prelude.hashWithSalt` regionsInfo
      `Prelude.hashWithSalt` shareMethod
      `Prelude.hashWithSalt` shareNotes
      `Prelude.hashWithSalt` shareStatus
      `Prelude.hashWithSalt` shortName
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` ssoEnabled
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` stageLastUpdatedDateTime
      `Prelude.hashWithSalt` stageReason
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcSettings

instance Prelude.NFData DirectoryDescription where
  rnf DirectoryDescription' {..} =
    Prelude.rnf accessUrl
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf connectSettings
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf desiredNumberOfDomainControllers
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf dnsIpAddrs
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf ownerDirectoryDescription
      `Prelude.seq` Prelude.rnf radiusSettings
      `Prelude.seq` Prelude.rnf radiusStatus
      `Prelude.seq` Prelude.rnf regionsInfo
      `Prelude.seq` Prelude.rnf shareMethod
      `Prelude.seq` Prelude.rnf shareNotes
      `Prelude.seq` Prelude.rnf shareStatus
      `Prelude.seq` Prelude.rnf shortName
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf ssoEnabled
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf
        stageLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf
        stageReason
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf
        vpcSettings
