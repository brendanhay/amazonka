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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryEdition
import Network.AWS.DirectoryService.Types.DirectorySize
import Network.AWS.DirectoryService.Types.DirectoryStage
import Network.AWS.DirectoryService.Types.DirectoryType
import Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import Network.AWS.DirectoryService.Types.RegionsInfo
import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import qualified Network.AWS.Lens as Lens

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'newDirectoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
  { -- | The status of the RADIUS MFA server connection.
    radiusStatus :: Core.Maybe RadiusStatus,
    -- | The alias for the directory. If no alias has been created for the
    -- directory, the alias is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    alias :: Core.Maybe Core.Text,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A DirectoryConnectSettingsDescription object that contains additional
    -- information about an AD Connector directory. This member is only present
    -- if the directory is an AD Connector directory.
    connectSettings :: Core.Maybe DirectoryConnectSettingsDescription,
    -- | A DirectoryVpcSettingsDescription object that contains additional
    -- information about a directory. This member is only present if the
    -- directory is a Simple AD or Managed AD directory.
    vpcSettings :: Core.Maybe DirectoryVpcSettingsDescription,
    -- | Additional information about the directory stage.
    stageReason :: Core.Maybe Core.Text,
    -- | Specifies when the directory was created.
    launchTime :: Core.Maybe Core.POSIX,
    -- | Lists the Regions where the directory has replicated.
    regionsInfo :: Core.Maybe RegionsInfo,
    -- | The short name of the directory.
    shortName :: Core.Maybe Core.Text,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your AWS organization
    -- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
    -- request (@HANDSHAKE@).
    shareMethod :: Core.Maybe ShareMethod,
    -- | The access URL for the directory, such as
    -- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
    -- directory, @\<alias>@ is the directory identifier, such as
    -- @d-XXXXXXXXXX@.
    accessUrl :: Core.Maybe Core.Text,
    -- | The fully qualified name of the directory.
    name :: Core.Maybe Core.Text,
    -- | The current stage of the directory.
    stage :: Core.Maybe DirectoryStage,
    -- | The edition associated with this directory.
    edition :: Core.Maybe DirectoryEdition,
    -- | The directory identifier.
    directoryId :: Core.Maybe Core.Text,
    -- | Current directory status of the shared AWS Managed Microsoft AD
    -- directory.
    shareStatus :: Core.Maybe ShareStatus,
    -- | Describes the AWS Managed Microsoft AD directory in the directory owner
    -- account.
    ownerDirectoryDescription :: Core.Maybe OwnerDirectoryDescription,
    -- | The description for the directory.
    description :: Core.Maybe Core.Text,
    -- | The directory size.
    type' :: Core.Maybe DirectoryType,
    -- | The IP addresses of the DNS servers for the directory. For a Simple AD
    -- or Microsoft AD directory, these are the IP addresses of the Simple AD
    -- or Microsoft AD directory servers. For an AD Connector directory, these
    -- are the IP addresses of the DNS servers or domain controllers in the
    -- on-premises directory to which the AD Connector is connected.
    dnsIpAddrs :: Core.Maybe [Core.Text],
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server configured for this directory.
    radiusSettings :: Core.Maybe RadiusSettings,
    -- | The desired number of domain controllers in the directory if the
    -- directory is Microsoft AD.
    desiredNumberOfDomainControllers :: Core.Maybe Core.Natural,
    -- | The directory size.
    size :: Core.Maybe DirectorySize,
    -- | The date and time that the stage was last updated.
    stageLastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | Indicates if single sign-on is enabled for the directory. For more
    -- information, see EnableSso and DisableSso.
    ssoEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'radiusStatus', 'directoryDescription_radiusStatus' - The status of the RADIUS MFA server connection.
--
-- 'alias', 'directoryDescription_alias' - The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'shareNotes', 'directoryDescription_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'connectSettings', 'directoryDescription_connectSettings' - A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
--
-- 'vpcSettings', 'directoryDescription_vpcSettings' - A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed AD directory.
--
-- 'stageReason', 'directoryDescription_stageReason' - Additional information about the directory stage.
--
-- 'launchTime', 'directoryDescription_launchTime' - Specifies when the directory was created.
--
-- 'regionsInfo', 'directoryDescription_regionsInfo' - Lists the Regions where the directory has replicated.
--
-- 'shortName', 'directoryDescription_shortName' - The short name of the directory.
--
-- 'shareMethod', 'directoryDescription_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
-- request (@HANDSHAKE@).
--
-- 'accessUrl', 'directoryDescription_accessUrl' - The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
--
-- 'name', 'directoryDescription_name' - The fully qualified name of the directory.
--
-- 'stage', 'directoryDescription_stage' - The current stage of the directory.
--
-- 'edition', 'directoryDescription_edition' - The edition associated with this directory.
--
-- 'directoryId', 'directoryDescription_directoryId' - The directory identifier.
--
-- 'shareStatus', 'directoryDescription_shareStatus' - Current directory status of the shared AWS Managed Microsoft AD
-- directory.
--
-- 'ownerDirectoryDescription', 'directoryDescription_ownerDirectoryDescription' - Describes the AWS Managed Microsoft AD directory in the directory owner
-- account.
--
-- 'description', 'directoryDescription_description' - The description for the directory.
--
-- 'type'', 'directoryDescription_type' - The directory size.
--
-- 'dnsIpAddrs', 'directoryDescription_dnsIpAddrs' - The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in the
-- on-premises directory to which the AD Connector is connected.
--
-- 'radiusSettings', 'directoryDescription_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
--
-- 'desiredNumberOfDomainControllers', 'directoryDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
--
-- 'size', 'directoryDescription_size' - The directory size.
--
-- 'stageLastUpdatedDateTime', 'directoryDescription_stageLastUpdatedDateTime' - The date and time that the stage was last updated.
--
-- 'ssoEnabled', 'directoryDescription_ssoEnabled' - Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
newDirectoryDescription ::
  DirectoryDescription
newDirectoryDescription =
  DirectoryDescription'
    { radiusStatus = Core.Nothing,
      alias = Core.Nothing,
      shareNotes = Core.Nothing,
      connectSettings = Core.Nothing,
      vpcSettings = Core.Nothing,
      stageReason = Core.Nothing,
      launchTime = Core.Nothing,
      regionsInfo = Core.Nothing,
      shortName = Core.Nothing,
      shareMethod = Core.Nothing,
      accessUrl = Core.Nothing,
      name = Core.Nothing,
      stage = Core.Nothing,
      edition = Core.Nothing,
      directoryId = Core.Nothing,
      shareStatus = Core.Nothing,
      ownerDirectoryDescription = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      dnsIpAddrs = Core.Nothing,
      radiusSettings = Core.Nothing,
      desiredNumberOfDomainControllers = Core.Nothing,
      size = Core.Nothing,
      stageLastUpdatedDateTime = Core.Nothing,
      ssoEnabled = Core.Nothing
    }

-- | The status of the RADIUS MFA server connection.
directoryDescription_radiusStatus :: Lens.Lens' DirectoryDescription (Core.Maybe RadiusStatus)
directoryDescription_radiusStatus = Lens.lens (\DirectoryDescription' {radiusStatus} -> radiusStatus) (\s@DirectoryDescription' {} a -> s {radiusStatus = a} :: DirectoryDescription)

-- | The alias for the directory. If no alias has been created for the
-- directory, the alias is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_alias :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_alias = Lens.lens (\DirectoryDescription' {alias} -> alias) (\s@DirectoryDescription' {} a -> s {alias = a} :: DirectoryDescription)

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
directoryDescription_shareNotes :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_shareNotes = Lens.lens (\DirectoryDescription' {shareNotes} -> shareNotes) (\s@DirectoryDescription' {} a -> s {shareNotes = a} :: DirectoryDescription) Core.. Lens.mapping Core._Sensitive

-- | A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
directoryDescription_connectSettings :: Lens.Lens' DirectoryDescription (Core.Maybe DirectoryConnectSettingsDescription)
directoryDescription_connectSettings = Lens.lens (\DirectoryDescription' {connectSettings} -> connectSettings) (\s@DirectoryDescription' {} a -> s {connectSettings = a} :: DirectoryDescription)

-- | A DirectoryVpcSettingsDescription object that contains additional
-- information about a directory. This member is only present if the
-- directory is a Simple AD or Managed AD directory.
directoryDescription_vpcSettings :: Lens.Lens' DirectoryDescription (Core.Maybe DirectoryVpcSettingsDescription)
directoryDescription_vpcSettings = Lens.lens (\DirectoryDescription' {vpcSettings} -> vpcSettings) (\s@DirectoryDescription' {} a -> s {vpcSettings = a} :: DirectoryDescription)

-- | Additional information about the directory stage.
directoryDescription_stageReason :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_stageReason = Lens.lens (\DirectoryDescription' {stageReason} -> stageReason) (\s@DirectoryDescription' {} a -> s {stageReason = a} :: DirectoryDescription)

-- | Specifies when the directory was created.
directoryDescription_launchTime :: Lens.Lens' DirectoryDescription (Core.Maybe Core.UTCTime)
directoryDescription_launchTime = Lens.lens (\DirectoryDescription' {launchTime} -> launchTime) (\s@DirectoryDescription' {} a -> s {launchTime = a} :: DirectoryDescription) Core.. Lens.mapping Core._Time

-- | Lists the Regions where the directory has replicated.
directoryDescription_regionsInfo :: Lens.Lens' DirectoryDescription (Core.Maybe RegionsInfo)
directoryDescription_regionsInfo = Lens.lens (\DirectoryDescription' {regionsInfo} -> regionsInfo) (\s@DirectoryDescription' {} a -> s {regionsInfo = a} :: DirectoryDescription)

-- | The short name of the directory.
directoryDescription_shortName :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_shortName = Lens.lens (\DirectoryDescription' {shortName} -> shortName) (\s@DirectoryDescription' {} a -> s {shortName = a} :: DirectoryDescription)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
-- request (@HANDSHAKE@).
directoryDescription_shareMethod :: Lens.Lens' DirectoryDescription (Core.Maybe ShareMethod)
directoryDescription_shareMethod = Lens.lens (\DirectoryDescription' {shareMethod} -> shareMethod) (\s@DirectoryDescription' {} a -> s {shareMethod = a} :: DirectoryDescription)

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@. If no alias has been created for the
-- directory, @\<alias>@ is the directory identifier, such as
-- @d-XXXXXXXXXX@.
directoryDescription_accessUrl :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_accessUrl = Lens.lens (\DirectoryDescription' {accessUrl} -> accessUrl) (\s@DirectoryDescription' {} a -> s {accessUrl = a} :: DirectoryDescription)

-- | The fully qualified name of the directory.
directoryDescription_name :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_name = Lens.lens (\DirectoryDescription' {name} -> name) (\s@DirectoryDescription' {} a -> s {name = a} :: DirectoryDescription)

-- | The current stage of the directory.
directoryDescription_stage :: Lens.Lens' DirectoryDescription (Core.Maybe DirectoryStage)
directoryDescription_stage = Lens.lens (\DirectoryDescription' {stage} -> stage) (\s@DirectoryDescription' {} a -> s {stage = a} :: DirectoryDescription)

-- | The edition associated with this directory.
directoryDescription_edition :: Lens.Lens' DirectoryDescription (Core.Maybe DirectoryEdition)
directoryDescription_edition = Lens.lens (\DirectoryDescription' {edition} -> edition) (\s@DirectoryDescription' {} a -> s {edition = a} :: DirectoryDescription)

-- | The directory identifier.
directoryDescription_directoryId :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_directoryId = Lens.lens (\DirectoryDescription' {directoryId} -> directoryId) (\s@DirectoryDescription' {} a -> s {directoryId = a} :: DirectoryDescription)

-- | Current directory status of the shared AWS Managed Microsoft AD
-- directory.
directoryDescription_shareStatus :: Lens.Lens' DirectoryDescription (Core.Maybe ShareStatus)
directoryDescription_shareStatus = Lens.lens (\DirectoryDescription' {shareStatus} -> shareStatus) (\s@DirectoryDescription' {} a -> s {shareStatus = a} :: DirectoryDescription)

-- | Describes the AWS Managed Microsoft AD directory in the directory owner
-- account.
directoryDescription_ownerDirectoryDescription :: Lens.Lens' DirectoryDescription (Core.Maybe OwnerDirectoryDescription)
directoryDescription_ownerDirectoryDescription = Lens.lens (\DirectoryDescription' {ownerDirectoryDescription} -> ownerDirectoryDescription) (\s@DirectoryDescription' {} a -> s {ownerDirectoryDescription = a} :: DirectoryDescription)

-- | The description for the directory.
directoryDescription_description :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Text)
directoryDescription_description = Lens.lens (\DirectoryDescription' {description} -> description) (\s@DirectoryDescription' {} a -> s {description = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_type :: Lens.Lens' DirectoryDescription (Core.Maybe DirectoryType)
directoryDescription_type = Lens.lens (\DirectoryDescription' {type'} -> type') (\s@DirectoryDescription' {} a -> s {type' = a} :: DirectoryDescription)

-- | The IP addresses of the DNS servers for the directory. For a Simple AD
-- or Microsoft AD directory, these are the IP addresses of the Simple AD
-- or Microsoft AD directory servers. For an AD Connector directory, these
-- are the IP addresses of the DNS servers or domain controllers in the
-- on-premises directory to which the AD Connector is connected.
directoryDescription_dnsIpAddrs :: Lens.Lens' DirectoryDescription (Core.Maybe [Core.Text])
directoryDescription_dnsIpAddrs = Lens.lens (\DirectoryDescription' {dnsIpAddrs} -> dnsIpAddrs) (\s@DirectoryDescription' {} a -> s {dnsIpAddrs = a} :: DirectoryDescription) Core.. Lens.mapping Lens._Coerce

-- | A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
directoryDescription_radiusSettings :: Lens.Lens' DirectoryDescription (Core.Maybe RadiusSettings)
directoryDescription_radiusSettings = Lens.lens (\DirectoryDescription' {radiusSettings} -> radiusSettings) (\s@DirectoryDescription' {} a -> s {radiusSettings = a} :: DirectoryDescription)

-- | The desired number of domain controllers in the directory if the
-- directory is Microsoft AD.
directoryDescription_desiredNumberOfDomainControllers :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Natural)
directoryDescription_desiredNumberOfDomainControllers = Lens.lens (\DirectoryDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@DirectoryDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: DirectoryDescription)

-- | The directory size.
directoryDescription_size :: Lens.Lens' DirectoryDescription (Core.Maybe DirectorySize)
directoryDescription_size = Lens.lens (\DirectoryDescription' {size} -> size) (\s@DirectoryDescription' {} a -> s {size = a} :: DirectoryDescription)

-- | The date and time that the stage was last updated.
directoryDescription_stageLastUpdatedDateTime :: Lens.Lens' DirectoryDescription (Core.Maybe Core.UTCTime)
directoryDescription_stageLastUpdatedDateTime = Lens.lens (\DirectoryDescription' {stageLastUpdatedDateTime} -> stageLastUpdatedDateTime) (\s@DirectoryDescription' {} a -> s {stageLastUpdatedDateTime = a} :: DirectoryDescription) Core.. Lens.mapping Core._Time

-- | Indicates if single sign-on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
directoryDescription_ssoEnabled :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Bool)
directoryDescription_ssoEnabled = Lens.lens (\DirectoryDescription' {ssoEnabled} -> ssoEnabled) (\s@DirectoryDescription' {} a -> s {ssoEnabled = a} :: DirectoryDescription)

instance Core.FromJSON DirectoryDescription where
  parseJSON =
    Core.withObject
      "DirectoryDescription"
      ( \x ->
          DirectoryDescription'
            Core.<$> (x Core..:? "RadiusStatus")
            Core.<*> (x Core..:? "Alias")
            Core.<*> (x Core..:? "ShareNotes")
            Core.<*> (x Core..:? "ConnectSettings")
            Core.<*> (x Core..:? "VpcSettings")
            Core.<*> (x Core..:? "StageReason")
            Core.<*> (x Core..:? "LaunchTime")
            Core.<*> (x Core..:? "RegionsInfo")
            Core.<*> (x Core..:? "ShortName")
            Core.<*> (x Core..:? "ShareMethod")
            Core.<*> (x Core..:? "AccessUrl")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Stage")
            Core.<*> (x Core..:? "Edition")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "ShareStatus")
            Core.<*> (x Core..:? "OwnerDirectoryDescription")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DnsIpAddrs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RadiusSettings")
            Core.<*> (x Core..:? "DesiredNumberOfDomainControllers")
            Core.<*> (x Core..:? "Size")
            Core.<*> (x Core..:? "StageLastUpdatedDateTime")
            Core.<*> (x Core..:? "SsoEnabled")
      )

instance Core.Hashable DirectoryDescription

instance Core.NFData DirectoryDescription
