{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryDescription
  ( DirectoryDescription (..),

    -- * Smart constructor
    mkDirectoryDescription,

    -- * Lenses
    ddAccessUrl,
    ddAlias,
    ddConnectSettings,
    ddDescription,
    ddDesiredNumberOfDomainControllers,
    ddDirectoryId,
    ddDnsIpAddrs,
    ddEdition,
    ddLaunchTime,
    ddName,
    ddOwnerDirectoryDescription,
    ddRadiusSettings,
    ddRadiusStatus,
    ddRegionsInfo,
    ddShareMethod,
    ddShareNotes,
    ddShareStatus,
    ddShortName,
    ddSize,
    ddSsoEnabled,
    ddStage,
    ddStageLastUpdatedDateTime,
    ddStageReason,
    ddType,
    ddVpcSettings,
  )
where

import qualified Network.AWS.DirectoryService.Types.AccessUrl as Types
import qualified Network.AWS.DirectoryService.Types.Alias as Types
import qualified Network.AWS.DirectoryService.Types.Description as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryEdition as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryShortName as Types
import qualified Network.AWS.DirectoryService.Types.DirectorySize as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryStage as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryType as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription as Types
import qualified Network.AWS.DirectoryService.Types.IpAddr as Types
import qualified Network.AWS.DirectoryService.Types.Name as Types
import qualified Network.AWS.DirectoryService.Types.OwnerDirectoryDescription as Types
import qualified Network.AWS.DirectoryService.Types.RadiusSettings as Types
import qualified Network.AWS.DirectoryService.Types.RadiusStatus as Types
import qualified Network.AWS.DirectoryService.Types.RegionsInfo as Types
import qualified Network.AWS.DirectoryService.Types.ShareMethod as Types
import qualified Network.AWS.DirectoryService.Types.ShareNotes as Types
import qualified Network.AWS.DirectoryService.Types.ShareStatus as Types
import qualified Network.AWS.DirectoryService.Types.StageReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'mkDirectoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
  { -- | The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
    accessUrl :: Core.Maybe Types.AccessUrl,
    -- | The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
    alias :: Core.Maybe Types.Alias,
    -- | A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
    connectSettings :: Core.Maybe Types.DirectoryConnectSettingsDescription,
    -- | The description for the directory.
    description :: Core.Maybe Types.Description,
    -- | The desired number of domain controllers in the directory if the directory is Microsoft AD.
    desiredNumberOfDomainControllers :: Core.Maybe Core.Natural,
    -- | The directory identifier.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
    dnsIpAddrs :: Core.Maybe [Types.IpAddr],
    -- | The edition associated with this directory.
    edition :: Core.Maybe Types.DirectoryEdition,
    -- | Specifies when the directory was created.
    launchTime :: Core.Maybe Core.NominalDiffTime,
    -- | The fully qualified name of the directory.
    name :: Core.Maybe Types.Name,
    -- | Describes the AWS Managed Microsoft AD directory in the directory owner account.
    ownerDirectoryDescription :: Core.Maybe Types.OwnerDirectoryDescription,
    -- | A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
    radiusSettings :: Core.Maybe Types.RadiusSettings,
    -- | The status of the RADIUS MFA server connection.
    radiusStatus :: Core.Maybe Types.RadiusStatus,
    -- | Lists the Regions where the directory has replicated.
    regionsInfo :: Core.Maybe Types.RegionsInfo,
    -- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
    shareMethod :: Core.Maybe Types.ShareMethod,
    -- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
    shareNotes :: Core.Maybe Types.ShareNotes,
    -- | Current directory status of the shared AWS Managed Microsoft AD directory.
    shareStatus :: Core.Maybe Types.ShareStatus,
    -- | The short name of the directory.
    shortName :: Core.Maybe Types.DirectoryShortName,
    -- | The directory size.
    size :: Core.Maybe Types.DirectorySize,
    -- | Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
    ssoEnabled :: Core.Maybe Core.Bool,
    -- | The current stage of the directory.
    stage :: Core.Maybe Types.DirectoryStage,
    -- | The date and time that the stage was last updated.
    stageLastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | Additional information about the directory stage.
    stageReason :: Core.Maybe Types.StageReason,
    -- | The directory size.
    type' :: Core.Maybe Types.DirectoryType,
    -- | A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
    vpcSettings :: Core.Maybe Types.DirectoryVpcSettingsDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DirectoryDescription' value with any optional fields omitted.
mkDirectoryDescription ::
  DirectoryDescription
mkDirectoryDescription =
  DirectoryDescription'
    { accessUrl = Core.Nothing,
      alias = Core.Nothing,
      connectSettings = Core.Nothing,
      description = Core.Nothing,
      desiredNumberOfDomainControllers = Core.Nothing,
      directoryId = Core.Nothing,
      dnsIpAddrs = Core.Nothing,
      edition = Core.Nothing,
      launchTime = Core.Nothing,
      name = Core.Nothing,
      ownerDirectoryDescription = Core.Nothing,
      radiusSettings = Core.Nothing,
      radiusStatus = Core.Nothing,
      regionsInfo = Core.Nothing,
      shareMethod = Core.Nothing,
      shareNotes = Core.Nothing,
      shareStatus = Core.Nothing,
      shortName = Core.Nothing,
      size = Core.Nothing,
      ssoEnabled = Core.Nothing,
      stage = Core.Nothing,
      stageLastUpdatedDateTime = Core.Nothing,
      stageReason = Core.Nothing,
      type' = Core.Nothing,
      vpcSettings = Core.Nothing
    }

-- | The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- /Note:/ Consider using 'accessUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAccessUrl :: Lens.Lens' DirectoryDescription (Core.Maybe Types.AccessUrl)
ddAccessUrl = Lens.field @"accessUrl"
{-# DEPRECATED ddAccessUrl "Use generic-lens or generic-optics with 'accessUrl' instead." #-}

-- | The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAlias :: Lens.Lens' DirectoryDescription (Core.Maybe Types.Alias)
ddAlias = Lens.field @"alias"
{-# DEPRECATED ddAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
--
-- /Note:/ Consider using 'connectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddConnectSettings :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryConnectSettingsDescription)
ddConnectSettings = Lens.field @"connectSettings"
{-# DEPRECATED ddConnectSettings "Use generic-lens or generic-optics with 'connectSettings' instead." #-}

-- | The description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDescription :: Lens.Lens' DirectoryDescription (Core.Maybe Types.Description)
ddDescription = Lens.field @"description"
{-# DEPRECATED ddDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The desired number of domain controllers in the directory if the directory is Microsoft AD.
--
-- /Note:/ Consider using 'desiredNumberOfDomainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDesiredNumberOfDomainControllers :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Natural)
ddDesiredNumberOfDomainControllers = Lens.field @"desiredNumberOfDomainControllers"
{-# DEPRECATED ddDesiredNumberOfDomainControllers "Use generic-lens or generic-optics with 'desiredNumberOfDomainControllers' instead." #-}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryId :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryId)
ddDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ddDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDnsIpAddrs :: Lens.Lens' DirectoryDescription (Core.Maybe [Types.IpAddr])
ddDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# DEPRECATED ddDnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead." #-}

-- | The edition associated with this directory.
--
-- /Note:/ Consider using 'edition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddEdition :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryEdition)
ddEdition = Lens.field @"edition"
{-# DEPRECATED ddEdition "Use generic-lens or generic-optics with 'edition' instead." #-}

-- | Specifies when the directory was created.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLaunchTime :: Lens.Lens' DirectoryDescription (Core.Maybe Core.NominalDiffTime)
ddLaunchTime = Lens.field @"launchTime"
{-# DEPRECATED ddLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The fully qualified name of the directory.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DirectoryDescription (Core.Maybe Types.Name)
ddName = Lens.field @"name"
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Describes the AWS Managed Microsoft AD directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddOwnerDirectoryDescription :: Lens.Lens' DirectoryDescription (Core.Maybe Types.OwnerDirectoryDescription)
ddOwnerDirectoryDescription = Lens.field @"ownerDirectoryDescription"
{-# DEPRECATED ddOwnerDirectoryDescription "Use generic-lens or generic-optics with 'ownerDirectoryDescription' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRadiusSettings :: Lens.Lens' DirectoryDescription (Core.Maybe Types.RadiusSettings)
ddRadiusSettings = Lens.field @"radiusSettings"
{-# DEPRECATED ddRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

-- | The status of the RADIUS MFA server connection.
--
-- /Note:/ Consider using 'radiusStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRadiusStatus :: Lens.Lens' DirectoryDescription (Core.Maybe Types.RadiusStatus)
ddRadiusStatus = Lens.field @"radiusStatus"
{-# DEPRECATED ddRadiusStatus "Use generic-lens or generic-optics with 'radiusStatus' instead." #-}

-- | Lists the Regions where the directory has replicated.
--
-- /Note:/ Consider using 'regionsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRegionsInfo :: Lens.Lens' DirectoryDescription (Core.Maybe Types.RegionsInfo)
ddRegionsInfo = Lens.field @"regionsInfo"
{-# DEPRECATED ddRegionsInfo "Use generic-lens or generic-optics with 'regionsInfo' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareMethod :: Lens.Lens' DirectoryDescription (Core.Maybe Types.ShareMethod)
ddShareMethod = Lens.field @"shareMethod"
{-# DEPRECATED ddShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareNotes :: Lens.Lens' DirectoryDescription (Core.Maybe Types.ShareNotes)
ddShareNotes = Lens.field @"shareNotes"
{-# DEPRECATED ddShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'shareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareStatus :: Lens.Lens' DirectoryDescription (Core.Maybe Types.ShareStatus)
ddShareStatus = Lens.field @"shareStatus"
{-# DEPRECATED ddShareStatus "Use generic-lens or generic-optics with 'shareStatus' instead." #-}

-- | The short name of the directory.
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShortName :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryShortName)
ddShortName = Lens.field @"shortName"
{-# DEPRECATED ddShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | The directory size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSize :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectorySize)
ddSize = Lens.field @"size"
{-# DEPRECATED ddSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
--
-- /Note:/ Consider using 'ssoEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSsoEnabled :: Lens.Lens' DirectoryDescription (Core.Maybe Core.Bool)
ddSsoEnabled = Lens.field @"ssoEnabled"
{-# DEPRECATED ddSsoEnabled "Use generic-lens or generic-optics with 'ssoEnabled' instead." #-}

-- | The current stage of the directory.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStage :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryStage)
ddStage = Lens.field @"stage"
{-# DEPRECATED ddStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The date and time that the stage was last updated.
--
-- /Note:/ Consider using 'stageLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStageLastUpdatedDateTime :: Lens.Lens' DirectoryDescription (Core.Maybe Core.NominalDiffTime)
ddStageLastUpdatedDateTime = Lens.field @"stageLastUpdatedDateTime"
{-# DEPRECATED ddStageLastUpdatedDateTime "Use generic-lens or generic-optics with 'stageLastUpdatedDateTime' instead." #-}

-- | Additional information about the directory stage.
--
-- /Note:/ Consider using 'stageReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStageReason :: Lens.Lens' DirectoryDescription (Core.Maybe Types.StageReason)
ddStageReason = Lens.field @"stageReason"
{-# DEPRECATED ddStageReason "Use generic-lens or generic-optics with 'stageReason' instead." #-}

-- | The directory size.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddType :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryType)
ddType = Lens.field @"type'"
{-# DEPRECATED ddType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddVpcSettings :: Lens.Lens' DirectoryDescription (Core.Maybe Types.DirectoryVpcSettingsDescription)
ddVpcSettings = Lens.field @"vpcSettings"
{-# DEPRECATED ddVpcSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

instance Core.FromJSON DirectoryDescription where
  parseJSON =
    Core.withObject "DirectoryDescription" Core.$
      \x ->
        DirectoryDescription'
          Core.<$> (x Core..:? "AccessUrl")
          Core.<*> (x Core..:? "Alias")
          Core.<*> (x Core..:? "ConnectSettings")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "DesiredNumberOfDomainControllers")
          Core.<*> (x Core..:? "DirectoryId")
          Core.<*> (x Core..:? "DnsIpAddrs")
          Core.<*> (x Core..:? "Edition")
          Core.<*> (x Core..:? "LaunchTime")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "OwnerDirectoryDescription")
          Core.<*> (x Core..:? "RadiusSettings")
          Core.<*> (x Core..:? "RadiusStatus")
          Core.<*> (x Core..:? "RegionsInfo")
          Core.<*> (x Core..:? "ShareMethod")
          Core.<*> (x Core..:? "ShareNotes")
          Core.<*> (x Core..:? "ShareStatus")
          Core.<*> (x Core..:? "ShortName")
          Core.<*> (x Core..:? "Size")
          Core.<*> (x Core..:? "SsoEnabled")
          Core.<*> (x Core..:? "Stage")
          Core.<*> (x Core..:? "StageLastUpdatedDateTime")
          Core.<*> (x Core..:? "StageReason")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "VpcSettings")
