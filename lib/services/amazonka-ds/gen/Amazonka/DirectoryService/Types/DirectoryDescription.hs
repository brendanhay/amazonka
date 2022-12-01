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
  { -- | The alias for the directory. If no alias has been created for the
    -- directory, the alias is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The directory identifier.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the directory.
    name :: Prelude.Maybe Prelude.Text,
    -- | Lists the Regions where the directory has replicated.
    regionsInfo :: Prelude.Maybe RegionsInfo,
    -- | The directory size.
    type' :: Prelude.Maybe DirectoryType,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies when the directory was created.
    launchTime :: Prelude.Maybe Core.POSIX,
    -- | Current directory status of the shared Managed Microsoft AD directory.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | The status of the RADIUS MFA server connection.
    radiusStatus :: Prelude.Maybe RadiusStatus,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your Amazon Web Services organization
    -- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
    -- shared directory request (@HANDSHAKE@).
    shareMethod :: Prelude.Maybe ShareMethod,
    -- | The directory size.
    size :: Prelude.Maybe DirectorySize,
    -- | A DirectoryVpcSettingsDescription object that contains additional
    -- information about a directory. This member is only present if the
    -- directory is a Simple AD or Managed Microsoft AD directory.
    vpcSettings :: Prelude.Maybe DirectoryVpcSettingsDescription,
    -- | The operating system (OS) version of the directory.
    osVersion :: Prelude.Maybe OSVersion,
    -- | Describes the Managed Microsoft AD directory in the directory owner
    -- account.
    ownerDirectoryDescription :: Prelude.Maybe OwnerDirectoryDescription,
    -- | The edition associated with this directory.
    edition :: Prelude.Maybe DirectoryEdition,
    -- | The description for the directory.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates if single sign-on is enabled for the directory. For more
    -- information, see EnableSso and DisableSso.
    ssoEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The access URL for the directory, such as
    -- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
    -- directory, @\<alias>@ is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    accessUrl :: Prelude.Maybe Prelude.Text,
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server configured for this directory.
    radiusSettings :: Prelude.Maybe RadiusSettings,
    -- | The current stage of the directory.
    stage :: Prelude.Maybe DirectoryStage,
    -- | The desired number of domain controllers in the directory if the
    -- directory is Microsoft AD.
    desiredNumberOfDomainControllers :: Prelude.Maybe Prelude.Natural,
    -- | A DirectoryConnectSettingsDescription object that contains additional
    -- information about an AD Connector directory. This member is only present
    -- if the directory is an AD Connector directory.
    connectSettings :: Prelude.Maybe DirectoryConnectSettingsDescription,
    -- | Additional information about the directory stage.
    stageReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the stage was last updated.
    stageLastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The IP addresses of the DNS servers for the directory. For a Simple AD
    -- or Microsoft AD directory, these are the IP addresses of the Simple AD
    -- or Microsoft AD directory servers. For an AD Connector directory, these
    -- are the IP addresses of the DNS servers or domain controllers in your
    -- self-managed directory to which the AD Connector is connected.
    dnsIpAddrs :: Prelude.Maybe [Prelude.Text],
    -- | The short name of the directory.
    shortName :: Prelude.Maybe Prelude.Text
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
-- 'alias', 'directoryDescription_alias' - The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'directoryId', 'directoryDescription_directoryId' - The directory identifier.
--
-- 'name', 'directoryDescription_name' - The fully qualified name of the directory.
--
-- 'regionsInfo', 'directoryDescription_regionsInfo' - Lists the Regions where the directory has replicated.
--
-- 'type'', 'directoryDescription_type' - The directory size.
--
-- 'shareNotes', 'directoryDescription_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'launchTime', 'directoryDescription_launchTime' - Specifies when the directory was created.
--
-- 'shareStatus', 'directoryDescription_shareStatus' - Current directory status of the shared Managed Microsoft AD directory.
--
-- 'radiusStatus', 'directoryDescription_radiusStatus' - The status of the RADIUS MFA server connection.
--
-- 'shareMethod', 'directoryDescription_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
--
-- 'size', 'directoryDescription_size' - The directory size.
--
-- 'vpcSettings', 'directoryDescription_vpcSettings' - A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed Microsoft AD directory.
--
-- 'osVersion', 'directoryDescription_osVersion' - The operating system (OS) version of the directory.
--
-- 'ownerDirectoryDescription', 'directoryDescription_ownerDirectoryDescription' - Describes the Managed Microsoft AD directory in the directory owner
-- account.
--
-- 'edition', 'directoryDescription_edition' - The edition associated with this directory.
--
-- 'description', 'directoryDescription_description' - The description for the directory.
--
-- 'ssoEnabled', 'directoryDescription_ssoEnabled' - Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
--
-- 'accessUrl', 'directoryDescription_accessUrl' - The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'radiusSettings', 'directoryDescription_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
--
-- 'stage', 'directoryDescription_stage' - The current stage of the directory.
--
-- 'desiredNumberOfDomainControllers', 'directoryDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
--
-- 'connectSettings', 'directoryDescription_connectSettings' - A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
--
-- 'stageReason', 'directoryDescription_stageReason' - Additional information about the directory stage.
--
-- 'stageLastUpdatedDateTime', 'directoryDescription_stageLastUpdatedDateTime' - The date and time that the stage was last updated.
--
-- 'dnsIpAddrs', 'directoryDescription_dnsIpAddrs' - The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in your
-- self-managed directory to which the AD Connector is connected.
--
-- 'shortName', 'directoryDescription_shortName' - The short name of the directory.
newDirectoryDescription ::
  DirectoryDescription
newDirectoryDescription =
  DirectoryDescription'
    { alias = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      name = Prelude.Nothing,
      regionsInfo = Prelude.Nothing,
      type' = Prelude.Nothing,
      shareNotes = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      radiusStatus = Prelude.Nothing,
      shareMethod = Prelude.Nothing,
      size = Prelude.Nothing,
      vpcSettings = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      ownerDirectoryDescription = Prelude.Nothing,
      edition = Prelude.Nothing,
      description = Prelude.Nothing,
      ssoEnabled = Prelude.Nothing,
      accessUrl = Prelude.Nothing,
      radiusSettings = Prelude.Nothing,
      stage = Prelude.Nothing,
      desiredNumberOfDomainControllers = Prelude.Nothing,
      connectSettings = Prelude.Nothing,
      stageReason = Prelude.Nothing,
      stageLastUpdatedDateTime = Prelude.Nothing,
      dnsIpAddrs = Prelude.Nothing,
      shortName = Prelude.Nothing
    }

-- | The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_alias :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_alias = Lens.lens (\DirectoryDescription' {alias} -> alias) (\s@DirectoryDescription' {} a -> s {alias = a} :: DirectoryDescription)

-- | The directory identifier.
directoryDescription_directoryId :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_directoryId = Lens.lens (\DirectoryDescription' {directoryId} -> directoryId) (\s@DirectoryDescription' {} a -> s {directoryId = a} :: DirectoryDescription)

-- | The fully qualified name of the directory.
directoryDescription_name :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_name = Lens.lens (\DirectoryDescription' {name} -> name) (\s@DirectoryDescription' {} a -> s {name = a} :: DirectoryDescription)

-- | Lists the Regions where the directory has replicated.
directoryDescription_regionsInfo :: Lens.Lens' DirectoryDescription (Prelude.Maybe RegionsInfo)
directoryDescription_regionsInfo = Lens.lens (\DirectoryDescription' {regionsInfo} -> regionsInfo) (\s@DirectoryDescription' {} a -> s {regionsInfo = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_type :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryType)
directoryDescription_type = Lens.lens (\DirectoryDescription' {type'} -> type') (\s@DirectoryDescription' {} a -> s {type' = a} :: DirectoryDescription)

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
directoryDescription_shareNotes :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_shareNotes = Lens.lens (\DirectoryDescription' {shareNotes} -> shareNotes) (\s@DirectoryDescription' {} a -> s {shareNotes = a} :: DirectoryDescription) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies when the directory was created.
directoryDescription_launchTime :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.UTCTime)
directoryDescription_launchTime = Lens.lens (\DirectoryDescription' {launchTime} -> launchTime) (\s@DirectoryDescription' {} a -> s {launchTime = a} :: DirectoryDescription) Prelude.. Lens.mapping Core._Time

-- | Current directory status of the shared Managed Microsoft AD directory.
directoryDescription_shareStatus :: Lens.Lens' DirectoryDescription (Prelude.Maybe ShareStatus)
directoryDescription_shareStatus = Lens.lens (\DirectoryDescription' {shareStatus} -> shareStatus) (\s@DirectoryDescription' {} a -> s {shareStatus = a} :: DirectoryDescription)

-- | The status of the RADIUS MFA server connection.
directoryDescription_radiusStatus :: Lens.Lens' DirectoryDescription (Prelude.Maybe RadiusStatus)
directoryDescription_radiusStatus = Lens.lens (\DirectoryDescription' {radiusStatus} -> radiusStatus) (\s@DirectoryDescription' {} a -> s {radiusStatus = a} :: DirectoryDescription)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
directoryDescription_shareMethod :: Lens.Lens' DirectoryDescription (Prelude.Maybe ShareMethod)
directoryDescription_shareMethod = Lens.lens (\DirectoryDescription' {shareMethod} -> shareMethod) (\s@DirectoryDescription' {} a -> s {shareMethod = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_size :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectorySize)
directoryDescription_size = Lens.lens (\DirectoryDescription' {size} -> size) (\s@DirectoryDescription' {} a -> s {size = a} :: DirectoryDescription)

-- | A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed Microsoft AD directory.
directoryDescription_vpcSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryVpcSettingsDescription)
directoryDescription_vpcSettings = Lens.lens (\DirectoryDescription' {vpcSettings} -> vpcSettings) (\s@DirectoryDescription' {} a -> s {vpcSettings = a} :: DirectoryDescription)

-- | The operating system (OS) version of the directory.
directoryDescription_osVersion :: Lens.Lens' DirectoryDescription (Prelude.Maybe OSVersion)
directoryDescription_osVersion = Lens.lens (\DirectoryDescription' {osVersion} -> osVersion) (\s@DirectoryDescription' {} a -> s {osVersion = a} :: DirectoryDescription)

-- | Describes the Managed Microsoft AD directory in the directory owner
-- account.
directoryDescription_ownerDirectoryDescription :: Lens.Lens' DirectoryDescription (Prelude.Maybe OwnerDirectoryDescription)
directoryDescription_ownerDirectoryDescription = Lens.lens (\DirectoryDescription' {ownerDirectoryDescription} -> ownerDirectoryDescription) (\s@DirectoryDescription' {} a -> s {ownerDirectoryDescription = a} :: DirectoryDescription)

-- | The edition associated with this directory.
directoryDescription_edition :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryEdition)
directoryDescription_edition = Lens.lens (\DirectoryDescription' {edition} -> edition) (\s@DirectoryDescription' {} a -> s {edition = a} :: DirectoryDescription)

-- | The description for the directory.
directoryDescription_description :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_description = Lens.lens (\DirectoryDescription' {description} -> description) (\s@DirectoryDescription' {} a -> s {description = a} :: DirectoryDescription)

-- | Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
directoryDescription_ssoEnabled :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Bool)
directoryDescription_ssoEnabled = Lens.lens (\DirectoryDescription' {ssoEnabled} -> ssoEnabled) (\s@DirectoryDescription' {} a -> s {ssoEnabled = a} :: DirectoryDescription)

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_accessUrl :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_accessUrl = Lens.lens (\DirectoryDescription' {accessUrl} -> accessUrl) (\s@DirectoryDescription' {} a -> s {accessUrl = a} :: DirectoryDescription)

-- | A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
directoryDescription_radiusSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe RadiusSettings)
directoryDescription_radiusSettings = Lens.lens (\DirectoryDescription' {radiusSettings} -> radiusSettings) (\s@DirectoryDescription' {} a -> s {radiusSettings = a} :: DirectoryDescription)

-- | The current stage of the directory.
directoryDescription_stage :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryStage)
directoryDescription_stage = Lens.lens (\DirectoryDescription' {stage} -> stage) (\s@DirectoryDescription' {} a -> s {stage = a} :: DirectoryDescription)

-- | The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
directoryDescription_desiredNumberOfDomainControllers :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Natural)
directoryDescription_desiredNumberOfDomainControllers = Lens.lens (\DirectoryDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@DirectoryDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: DirectoryDescription)

-- | A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
directoryDescription_connectSettings :: Lens.Lens' DirectoryDescription (Prelude.Maybe DirectoryConnectSettingsDescription)
directoryDescription_connectSettings = Lens.lens (\DirectoryDescription' {connectSettings} -> connectSettings) (\s@DirectoryDescription' {} a -> s {connectSettings = a} :: DirectoryDescription)

-- | Additional information about the directory stage.
directoryDescription_stageReason :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_stageReason = Lens.lens (\DirectoryDescription' {stageReason} -> stageReason) (\s@DirectoryDescription' {} a -> s {stageReason = a} :: DirectoryDescription)

-- | The date and time that the stage was last updated.
directoryDescription_stageLastUpdatedDateTime :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.UTCTime)
directoryDescription_stageLastUpdatedDateTime = Lens.lens (\DirectoryDescription' {stageLastUpdatedDateTime} -> stageLastUpdatedDateTime) (\s@DirectoryDescription' {} a -> s {stageLastUpdatedDateTime = a} :: DirectoryDescription) Prelude.. Lens.mapping Core._Time

-- | The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in your
-- self-managed directory to which the AD Connector is connected.
directoryDescription_dnsIpAddrs :: Lens.Lens' DirectoryDescription (Prelude.Maybe [Prelude.Text])
directoryDescription_dnsIpAddrs = Lens.lens (\DirectoryDescription' {dnsIpAddrs} -> dnsIpAddrs) (\s@DirectoryDescription' {} a -> s {dnsIpAddrs = a} :: DirectoryDescription) Prelude.. Lens.mapping Lens.coerced

-- | The short name of the directory.
directoryDescription_shortName :: Lens.Lens' DirectoryDescription (Prelude.Maybe Prelude.Text)
directoryDescription_shortName = Lens.lens (\DirectoryDescription' {shortName} -> shortName) (\s@DirectoryDescription' {} a -> s {shortName = a} :: DirectoryDescription)

instance Core.FromJSON DirectoryDescription where
  parseJSON =
    Core.withObject
      "DirectoryDescription"
      ( \x ->
          DirectoryDescription'
            Prelude.<$> (x Core..:? "Alias")
            Prelude.<*> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RegionsInfo")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ShareNotes")
            Prelude.<*> (x Core..:? "LaunchTime")
            Prelude.<*> (x Core..:? "ShareStatus")
            Prelude.<*> (x Core..:? "RadiusStatus")
            Prelude.<*> (x Core..:? "ShareMethod")
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "VpcSettings")
            Prelude.<*> (x Core..:? "OsVersion")
            Prelude.<*> (x Core..:? "OwnerDirectoryDescription")
            Prelude.<*> (x Core..:? "Edition")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SsoEnabled")
            Prelude.<*> (x Core..:? "AccessUrl")
            Prelude.<*> (x Core..:? "RadiusSettings")
            Prelude.<*> (x Core..:? "Stage")
            Prelude.<*> (x Core..:? "DesiredNumberOfDomainControllers")
            Prelude.<*> (x Core..:? "ConnectSettings")
            Prelude.<*> (x Core..:? "StageReason")
            Prelude.<*> (x Core..:? "StageLastUpdatedDateTime")
            Prelude.<*> (x Core..:? "DnsIpAddrs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ShortName")
      )

instance Prelude.Hashable DirectoryDescription where
  hashWithSalt _salt DirectoryDescription' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regionsInfo
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` shareNotes
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` shareStatus
      `Prelude.hashWithSalt` radiusStatus
      `Prelude.hashWithSalt` shareMethod
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` vpcSettings
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` ownerDirectoryDescription
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ssoEnabled
      `Prelude.hashWithSalt` accessUrl
      `Prelude.hashWithSalt` radiusSettings
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` desiredNumberOfDomainControllers
      `Prelude.hashWithSalt` connectSettings
      `Prelude.hashWithSalt` stageReason
      `Prelude.hashWithSalt` stageLastUpdatedDateTime
      `Prelude.hashWithSalt` dnsIpAddrs
      `Prelude.hashWithSalt` shortName

instance Prelude.NFData DirectoryDescription where
  rnf DirectoryDescription' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regionsInfo
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf shareNotes
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf shareStatus
      `Prelude.seq` Prelude.rnf radiusStatus
      `Prelude.seq` Prelude.rnf shareMethod
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf vpcSettings
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf ownerDirectoryDescription
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ssoEnabled
      `Prelude.seq` Prelude.rnf accessUrl
      `Prelude.seq` Prelude.rnf radiusSettings
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf
        desiredNumberOfDomainControllers
      `Prelude.seq` Prelude.rnf
        connectSettings
      `Prelude.seq` Prelude.rnf stageReason
      `Prelude.seq` Prelude.rnf
        stageLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf
        dnsIpAddrs
      `Prelude.seq` Prelude.rnf
        shortName
