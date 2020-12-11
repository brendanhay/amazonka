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
    ddEdition,
    ddRadiusStatus,
    ddStage,
    ddDirectoryId,
    ddAccessURL,
    ddShortName,
    ddRegionsInfo,
    ddSize,
    ddDesiredNumberOfDomainControllers,
    ddRadiusSettings,
    ddLaunchTime,
    ddAlias,
    ddShareStatus,
    ddName,
    ddShareMethod,
    ddStageLastUpdatedDateTime,
    ddSSOEnabled,
    ddDNSIPAddrs,
    ddVPCSettings,
    ddType,
    ddStageReason,
    ddConnectSettings,
    ddOwnerDirectoryDescription,
    ddDescription,
    ddShareNotes,
  )
where

import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryEdition
import Network.AWS.DirectoryService.Types.DirectorySize
import Network.AWS.DirectoryService.Types.DirectoryStage
import Network.AWS.DirectoryService.Types.DirectoryType
import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import Network.AWS.DirectoryService.Types.RegionsInfo
import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'mkDirectoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
  { edition ::
      Lude.Maybe DirectoryEdition,
    radiusStatus :: Lude.Maybe RadiusStatus,
    stage :: Lude.Maybe DirectoryStage,
    directoryId :: Lude.Maybe Lude.Text,
    accessURL :: Lude.Maybe Lude.Text,
    shortName :: Lude.Maybe Lude.Text,
    regionsInfo :: Lude.Maybe RegionsInfo,
    size :: Lude.Maybe DirectorySize,
    desiredNumberOfDomainControllers ::
      Lude.Maybe Lude.Natural,
    radiusSettings :: Lude.Maybe RadiusSettings,
    launchTime :: Lude.Maybe Lude.Timestamp,
    alias :: Lude.Maybe Lude.Text,
    shareStatus :: Lude.Maybe ShareStatus,
    name :: Lude.Maybe Lude.Text,
    shareMethod :: Lude.Maybe ShareMethod,
    stageLastUpdatedDateTime ::
      Lude.Maybe Lude.Timestamp,
    ssoEnabled :: Lude.Maybe Lude.Bool,
    dnsIPAddrs :: Lude.Maybe [Lude.Text],
    vpcSettings ::
      Lude.Maybe DirectoryVPCSettingsDescription,
    type' :: Lude.Maybe DirectoryType,
    stageReason :: Lude.Maybe Lude.Text,
    connectSettings ::
      Lude.Maybe DirectoryConnectSettingsDescription,
    ownerDirectoryDescription ::
      Lude.Maybe OwnerDirectoryDescription,
    description :: Lude.Maybe Lude.Text,
    shareNotes ::
      Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryDescription' with the minimum fields required to make a request.
--
-- * 'accessURL' - The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
-- * 'alias' - The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
-- * 'connectSettings' - A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
-- * 'description' - The description for the directory.
-- * 'desiredNumberOfDomainControllers' - The desired number of domain controllers in the directory if the directory is Microsoft AD.
-- * 'directoryId' - The directory identifier.
-- * 'dnsIPAddrs' - The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
-- * 'edition' - The edition associated with this directory.
-- * 'launchTime' - Specifies when the directory was created.
-- * 'name' - The fully qualified name of the directory.
-- * 'ownerDirectoryDescription' - Describes the AWS Managed Microsoft AD directory in the directory owner account.
-- * 'radiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
-- * 'radiusStatus' - The status of the RADIUS MFA server connection.
-- * 'regionsInfo' - Lists the Regions where the directory has replicated.
-- * 'shareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
-- * 'shareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
-- * 'shareStatus' - Current directory status of the shared AWS Managed Microsoft AD directory.
-- * 'shortName' - The short name of the directory.
-- * 'size' - The directory size.
-- * 'ssoEnabled' - Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
-- * 'stage' - The current stage of the directory.
-- * 'stageLastUpdatedDateTime' - The date and time that the stage was last updated.
-- * 'stageReason' - Additional information about the directory stage.
-- * 'type'' - The directory size.
-- * 'vpcSettings' - A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
mkDirectoryDescription ::
  DirectoryDescription
mkDirectoryDescription =
  DirectoryDescription'
    { edition = Lude.Nothing,
      radiusStatus = Lude.Nothing,
      stage = Lude.Nothing,
      directoryId = Lude.Nothing,
      accessURL = Lude.Nothing,
      shortName = Lude.Nothing,
      regionsInfo = Lude.Nothing,
      size = Lude.Nothing,
      desiredNumberOfDomainControllers = Lude.Nothing,
      radiusSettings = Lude.Nothing,
      launchTime = Lude.Nothing,
      alias = Lude.Nothing,
      shareStatus = Lude.Nothing,
      name = Lude.Nothing,
      shareMethod = Lude.Nothing,
      stageLastUpdatedDateTime = Lude.Nothing,
      ssoEnabled = Lude.Nothing,
      dnsIPAddrs = Lude.Nothing,
      vpcSettings = Lude.Nothing,
      type' = Lude.Nothing,
      stageReason = Lude.Nothing,
      connectSettings = Lude.Nothing,
      ownerDirectoryDescription = Lude.Nothing,
      description = Lude.Nothing,
      shareNotes = Lude.Nothing
    }

-- | The edition associated with this directory.
--
-- /Note:/ Consider using 'edition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddEdition :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectoryEdition)
ddEdition = Lens.lens (edition :: DirectoryDescription -> Lude.Maybe DirectoryEdition) (\s a -> s {edition = a} :: DirectoryDescription)
{-# DEPRECATED ddEdition "Use generic-lens or generic-optics with 'edition' instead." #-}

-- | The status of the RADIUS MFA server connection.
--
-- /Note:/ Consider using 'radiusStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRadiusStatus :: Lens.Lens' DirectoryDescription (Lude.Maybe RadiusStatus)
ddRadiusStatus = Lens.lens (radiusStatus :: DirectoryDescription -> Lude.Maybe RadiusStatus) (\s a -> s {radiusStatus = a} :: DirectoryDescription)
{-# DEPRECATED ddRadiusStatus "Use generic-lens or generic-optics with 'radiusStatus' instead." #-}

-- | The current stage of the directory.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStage :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectoryStage)
ddStage = Lens.lens (stage :: DirectoryDescription -> Lude.Maybe DirectoryStage) (\s a -> s {stage = a} :: DirectoryDescription)
{-# DEPRECATED ddStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryId :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddDirectoryId = Lens.lens (directoryId :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DirectoryDescription)
{-# DEPRECATED ddDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- /Note:/ Consider using 'accessURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAccessURL :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddAccessURL = Lens.lens (accessURL :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {accessURL = a} :: DirectoryDescription)
{-# DEPRECATED ddAccessURL "Use generic-lens or generic-optics with 'accessURL' instead." #-}

-- | The short name of the directory.
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShortName :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddShortName = Lens.lens (shortName :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {shortName = a} :: DirectoryDescription)
{-# DEPRECATED ddShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | Lists the Regions where the directory has replicated.
--
-- /Note:/ Consider using 'regionsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRegionsInfo :: Lens.Lens' DirectoryDescription (Lude.Maybe RegionsInfo)
ddRegionsInfo = Lens.lens (regionsInfo :: DirectoryDescription -> Lude.Maybe RegionsInfo) (\s a -> s {regionsInfo = a} :: DirectoryDescription)
{-# DEPRECATED ddRegionsInfo "Use generic-lens or generic-optics with 'regionsInfo' instead." #-}

-- | The directory size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSize :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectorySize)
ddSize = Lens.lens (size :: DirectoryDescription -> Lude.Maybe DirectorySize) (\s a -> s {size = a} :: DirectoryDescription)
{-# DEPRECATED ddSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The desired number of domain controllers in the directory if the directory is Microsoft AD.
--
-- /Note:/ Consider using 'desiredNumberOfDomainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDesiredNumberOfDomainControllers :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Natural)
ddDesiredNumberOfDomainControllers = Lens.lens (desiredNumberOfDomainControllers :: DirectoryDescription -> Lude.Maybe Lude.Natural) (\s a -> s {desiredNumberOfDomainControllers = a} :: DirectoryDescription)
{-# DEPRECATED ddDesiredNumberOfDomainControllers "Use generic-lens or generic-optics with 'desiredNumberOfDomainControllers' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRadiusSettings :: Lens.Lens' DirectoryDescription (Lude.Maybe RadiusSettings)
ddRadiusSettings = Lens.lens (radiusSettings :: DirectoryDescription -> Lude.Maybe RadiusSettings) (\s a -> s {radiusSettings = a} :: DirectoryDescription)
{-# DEPRECATED ddRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

-- | Specifies when the directory was created.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLaunchTime :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Timestamp)
ddLaunchTime = Lens.lens (launchTime :: DirectoryDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {launchTime = a} :: DirectoryDescription)
{-# DEPRECATED ddLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAlias :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddAlias = Lens.lens (alias :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {alias = a} :: DirectoryDescription)
{-# DEPRECATED ddAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'shareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareStatus :: Lens.Lens' DirectoryDescription (Lude.Maybe ShareStatus)
ddShareStatus = Lens.lens (shareStatus :: DirectoryDescription -> Lude.Maybe ShareStatus) (\s a -> s {shareStatus = a} :: DirectoryDescription)
{-# DEPRECATED ddShareStatus "Use generic-lens or generic-optics with 'shareStatus' instead." #-}

-- | The fully qualified name of the directory.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddName = Lens.lens (name :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DirectoryDescription)
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareMethod :: Lens.Lens' DirectoryDescription (Lude.Maybe ShareMethod)
ddShareMethod = Lens.lens (shareMethod :: DirectoryDescription -> Lude.Maybe ShareMethod) (\s a -> s {shareMethod = a} :: DirectoryDescription)
{-# DEPRECATED ddShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | The date and time that the stage was last updated.
--
-- /Note:/ Consider using 'stageLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStageLastUpdatedDateTime :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Timestamp)
ddStageLastUpdatedDateTime = Lens.lens (stageLastUpdatedDateTime :: DirectoryDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {stageLastUpdatedDateTime = a} :: DirectoryDescription)
{-# DEPRECATED ddStageLastUpdatedDateTime "Use generic-lens or generic-optics with 'stageLastUpdatedDateTime' instead." #-}

-- | Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
--
-- /Note:/ Consider using 'ssoEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSSOEnabled :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Bool)
ddSSOEnabled = Lens.lens (ssoEnabled :: DirectoryDescription -> Lude.Maybe Lude.Bool) (\s a -> s {ssoEnabled = a} :: DirectoryDescription)
{-# DEPRECATED ddSSOEnabled "Use generic-lens or generic-optics with 'ssoEnabled' instead." #-}

-- | The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
--
-- /Note:/ Consider using 'dnsIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDNSIPAddrs :: Lens.Lens' DirectoryDescription (Lude.Maybe [Lude.Text])
ddDNSIPAddrs = Lens.lens (dnsIPAddrs :: DirectoryDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsIPAddrs = a} :: DirectoryDescription)
{-# DEPRECATED ddDNSIPAddrs "Use generic-lens or generic-optics with 'dnsIPAddrs' instead." #-}

-- | A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddVPCSettings :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectoryVPCSettingsDescription)
ddVPCSettings = Lens.lens (vpcSettings :: DirectoryDescription -> Lude.Maybe DirectoryVPCSettingsDescription) (\s a -> s {vpcSettings = a} :: DirectoryDescription)
{-# DEPRECATED ddVPCSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

-- | The directory size.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddType :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectoryType)
ddType = Lens.lens (type' :: DirectoryDescription -> Lude.Maybe DirectoryType) (\s a -> s {type' = a} :: DirectoryDescription)
{-# DEPRECATED ddType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Additional information about the directory stage.
--
-- /Note:/ Consider using 'stageReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStageReason :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddStageReason = Lens.lens (stageReason :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {stageReason = a} :: DirectoryDescription)
{-# DEPRECATED ddStageReason "Use generic-lens or generic-optics with 'stageReason' instead." #-}

-- | A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
--
-- /Note:/ Consider using 'connectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddConnectSettings :: Lens.Lens' DirectoryDescription (Lude.Maybe DirectoryConnectSettingsDescription)
ddConnectSettings = Lens.lens (connectSettings :: DirectoryDescription -> Lude.Maybe DirectoryConnectSettingsDescription) (\s a -> s {connectSettings = a} :: DirectoryDescription)
{-# DEPRECATED ddConnectSettings "Use generic-lens or generic-optics with 'connectSettings' instead." #-}

-- | Describes the AWS Managed Microsoft AD directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddOwnerDirectoryDescription :: Lens.Lens' DirectoryDescription (Lude.Maybe OwnerDirectoryDescription)
ddOwnerDirectoryDescription = Lens.lens (ownerDirectoryDescription :: DirectoryDescription -> Lude.Maybe OwnerDirectoryDescription) (\s a -> s {ownerDirectoryDescription = a} :: DirectoryDescription)
{-# DEPRECATED ddOwnerDirectoryDescription "Use generic-lens or generic-optics with 'ownerDirectoryDescription' instead." #-}

-- | The description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDescription :: Lens.Lens' DirectoryDescription (Lude.Maybe Lude.Text)
ddDescription = Lens.lens (description :: DirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DirectoryDescription)
{-# DEPRECATED ddDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddShareNotes :: Lens.Lens' DirectoryDescription (Lude.Maybe (Lude.Sensitive Lude.Text))
ddShareNotes = Lens.lens (shareNotes :: DirectoryDescription -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {shareNotes = a} :: DirectoryDescription)
{-# DEPRECATED ddShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

instance Lude.FromJSON DirectoryDescription where
  parseJSON =
    Lude.withObject
      "DirectoryDescription"
      ( \x ->
          DirectoryDescription'
            Lude.<$> (x Lude..:? "Edition")
            Lude.<*> (x Lude..:? "RadiusStatus")
            Lude.<*> (x Lude..:? "Stage")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "AccessUrl")
            Lude.<*> (x Lude..:? "ShortName")
            Lude.<*> (x Lude..:? "RegionsInfo")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "DesiredNumberOfDomainControllers")
            Lude.<*> (x Lude..:? "RadiusSettings")
            Lude.<*> (x Lude..:? "LaunchTime")
            Lude.<*> (x Lude..:? "Alias")
            Lude.<*> (x Lude..:? "ShareStatus")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ShareMethod")
            Lude.<*> (x Lude..:? "StageLastUpdatedDateTime")
            Lude.<*> (x Lude..:? "SsoEnabled")
            Lude.<*> (x Lude..:? "DnsIpAddrs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcSettings")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "StageReason")
            Lude.<*> (x Lude..:? "ConnectSettings")
            Lude.<*> (x Lude..:? "OwnerDirectoryDescription")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ShareNotes")
      )
