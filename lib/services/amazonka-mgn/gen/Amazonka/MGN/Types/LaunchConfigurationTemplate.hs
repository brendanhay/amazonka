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
-- Module      : Amazonka.MGN.Types.LaunchConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchConfigurationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.BootMode
import Amazonka.MGN.Types.LaunchDisposition
import Amazonka.MGN.Types.LaunchTemplateDiskConf
import Amazonka.MGN.Types.Licensing
import Amazonka.MGN.Types.PostLaunchActions
import Amazonka.MGN.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfigurationTemplate' smart constructor.
data LaunchConfigurationTemplate = LaunchConfigurationTemplate'
  { -- | ARN of the Launch Configuration Template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Associate public Ip address.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Launch configuration template boot mode.
    bootMode :: Prelude.Maybe BootMode,
    -- | Copy private Ip.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Copy tags.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | EC2 launch template ID.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Enable map auto tagging.
    enableMapAutoTagging :: Prelude.Maybe Prelude.Bool,
    -- | Large volume config.
    largeVolumeConf :: Prelude.Maybe LaunchTemplateDiskConf,
    -- | Launch disposition.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    licensing :: Prelude.Maybe Licensing,
    -- | Launch configuration template map auto tagging MPE ID.
    mapAutoTaggingMpeID :: Prelude.Maybe Prelude.Text,
    -- | Post Launch Actions of the Launch Configuration Template.
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Small volume config.
    smallVolumeConf :: Prelude.Maybe LaunchTemplateDiskConf,
    -- | Small volume maximum size.
    smallVolumeMaxSize :: Prelude.Maybe Prelude.Natural,
    -- | Tags of the Launch Configuration Template.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | ID of the Launch Configuration Template.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'launchConfigurationTemplate_arn' - ARN of the Launch Configuration Template.
--
-- 'associatePublicIpAddress', 'launchConfigurationTemplate_associatePublicIpAddress' - Associate public Ip address.
--
-- 'bootMode', 'launchConfigurationTemplate_bootMode' - Launch configuration template boot mode.
--
-- 'copyPrivateIp', 'launchConfigurationTemplate_copyPrivateIp' - Copy private Ip.
--
-- 'copyTags', 'launchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'ec2LaunchTemplateID', 'launchConfigurationTemplate_ec2LaunchTemplateID' - EC2 launch template ID.
--
-- 'enableMapAutoTagging', 'launchConfigurationTemplate_enableMapAutoTagging' - Enable map auto tagging.
--
-- 'largeVolumeConf', 'launchConfigurationTemplate_largeVolumeConf' - Large volume config.
--
-- 'launchDisposition', 'launchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'launchConfigurationTemplate_licensing' - Undocumented member.
--
-- 'mapAutoTaggingMpeID', 'launchConfigurationTemplate_mapAutoTaggingMpeID' - Launch configuration template map auto tagging MPE ID.
--
-- 'postLaunchActions', 'launchConfigurationTemplate_postLaunchActions' - Post Launch Actions of the Launch Configuration Template.
--
-- 'smallVolumeConf', 'launchConfigurationTemplate_smallVolumeConf' - Small volume config.
--
-- 'smallVolumeMaxSize', 'launchConfigurationTemplate_smallVolumeMaxSize' - Small volume maximum size.
--
-- 'tags', 'launchConfigurationTemplate_tags' - Tags of the Launch Configuration Template.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
--
-- 'launchConfigurationTemplateID', 'launchConfigurationTemplate_launchConfigurationTemplateID' - ID of the Launch Configuration Template.
newLaunchConfigurationTemplate ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  LaunchConfigurationTemplate
newLaunchConfigurationTemplate
  pLaunchConfigurationTemplateID_ =
    LaunchConfigurationTemplate'
      { arn = Prelude.Nothing,
        associatePublicIpAddress = Prelude.Nothing,
        bootMode = Prelude.Nothing,
        copyPrivateIp = Prelude.Nothing,
        copyTags = Prelude.Nothing,
        ec2LaunchTemplateID = Prelude.Nothing,
        enableMapAutoTagging = Prelude.Nothing,
        largeVolumeConf = Prelude.Nothing,
        launchDisposition = Prelude.Nothing,
        licensing = Prelude.Nothing,
        mapAutoTaggingMpeID = Prelude.Nothing,
        postLaunchActions = Prelude.Nothing,
        smallVolumeConf = Prelude.Nothing,
        smallVolumeMaxSize = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetInstanceTypeRightSizingMethod =
          Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | ARN of the Launch Configuration Template.
launchConfigurationTemplate_arn :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_arn = Lens.lens (\LaunchConfigurationTemplate' {arn} -> arn) (\s@LaunchConfigurationTemplate' {} a -> s {arn = a} :: LaunchConfigurationTemplate)

-- | Associate public Ip address.
launchConfigurationTemplate_associatePublicIpAddress :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_associatePublicIpAddress = Lens.lens (\LaunchConfigurationTemplate' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchConfigurationTemplate' {} a -> s {associatePublicIpAddress = a} :: LaunchConfigurationTemplate)

-- | Launch configuration template boot mode.
launchConfigurationTemplate_bootMode :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe BootMode)
launchConfigurationTemplate_bootMode = Lens.lens (\LaunchConfigurationTemplate' {bootMode} -> bootMode) (\s@LaunchConfigurationTemplate' {} a -> s {bootMode = a} :: LaunchConfigurationTemplate)

-- | Copy private Ip.
launchConfigurationTemplate_copyPrivateIp :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_copyPrivateIp = Lens.lens (\LaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@LaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: LaunchConfigurationTemplate)

-- | Copy tags.
launchConfigurationTemplate_copyTags :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_copyTags = Lens.lens (\LaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@LaunchConfigurationTemplate' {} a -> s {copyTags = a} :: LaunchConfigurationTemplate)

-- | EC2 launch template ID.
launchConfigurationTemplate_ec2LaunchTemplateID :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_ec2LaunchTemplateID = Lens.lens (\LaunchConfigurationTemplate' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@LaunchConfigurationTemplate' {} a -> s {ec2LaunchTemplateID = a} :: LaunchConfigurationTemplate)

-- | Enable map auto tagging.
launchConfigurationTemplate_enableMapAutoTagging :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
launchConfigurationTemplate_enableMapAutoTagging = Lens.lens (\LaunchConfigurationTemplate' {enableMapAutoTagging} -> enableMapAutoTagging) (\s@LaunchConfigurationTemplate' {} a -> s {enableMapAutoTagging = a} :: LaunchConfigurationTemplate)

-- | Large volume config.
launchConfigurationTemplate_largeVolumeConf :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
launchConfigurationTemplate_largeVolumeConf = Lens.lens (\LaunchConfigurationTemplate' {largeVolumeConf} -> largeVolumeConf) (\s@LaunchConfigurationTemplate' {} a -> s {largeVolumeConf = a} :: LaunchConfigurationTemplate)

-- | Launch disposition.
launchConfigurationTemplate_launchDisposition :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
launchConfigurationTemplate_launchDisposition = Lens.lens (\LaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@LaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: LaunchConfigurationTemplate)

-- | Undocumented member.
launchConfigurationTemplate_licensing :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Licensing)
launchConfigurationTemplate_licensing = Lens.lens (\LaunchConfigurationTemplate' {licensing} -> licensing) (\s@LaunchConfigurationTemplate' {} a -> s {licensing = a} :: LaunchConfigurationTemplate)

-- | Launch configuration template map auto tagging MPE ID.
launchConfigurationTemplate_mapAutoTaggingMpeID :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_mapAutoTaggingMpeID = Lens.lens (\LaunchConfigurationTemplate' {mapAutoTaggingMpeID} -> mapAutoTaggingMpeID) (\s@LaunchConfigurationTemplate' {} a -> s {mapAutoTaggingMpeID = a} :: LaunchConfigurationTemplate)

-- | Post Launch Actions of the Launch Configuration Template.
launchConfigurationTemplate_postLaunchActions :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
launchConfigurationTemplate_postLaunchActions = Lens.lens (\LaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@LaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: LaunchConfigurationTemplate)

-- | Small volume config.
launchConfigurationTemplate_smallVolumeConf :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
launchConfigurationTemplate_smallVolumeConf = Lens.lens (\LaunchConfigurationTemplate' {smallVolumeConf} -> smallVolumeConf) (\s@LaunchConfigurationTemplate' {} a -> s {smallVolumeConf = a} :: LaunchConfigurationTemplate)

-- | Small volume maximum size.
launchConfigurationTemplate_smallVolumeMaxSize :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Natural)
launchConfigurationTemplate_smallVolumeMaxSize = Lens.lens (\LaunchConfigurationTemplate' {smallVolumeMaxSize} -> smallVolumeMaxSize) (\s@LaunchConfigurationTemplate' {} a -> s {smallVolumeMaxSize = a} :: LaunchConfigurationTemplate)

-- | Tags of the Launch Configuration Template.
launchConfigurationTemplate_tags :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchConfigurationTemplate_tags = Lens.lens (\LaunchConfigurationTemplate' {tags} -> tags) (\s@LaunchConfigurationTemplate' {} a -> s {tags = a} :: LaunchConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Target instance type right-sizing method.
launchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfigurationTemplate)

-- | ID of the Launch Configuration Template.
launchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' LaunchConfigurationTemplate Prelude.Text
launchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\LaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@LaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: LaunchConfigurationTemplate)

instance Data.FromJSON LaunchConfigurationTemplate where
  parseJSON =
    Data.withObject
      "LaunchConfigurationTemplate"
      ( \x ->
          LaunchConfigurationTemplate'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "associatePublicIpAddress")
            Prelude.<*> (x Data..:? "bootMode")
            Prelude.<*> (x Data..:? "copyPrivateIp")
            Prelude.<*> (x Data..:? "copyTags")
            Prelude.<*> (x Data..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Data..:? "enableMapAutoTagging")
            Prelude.<*> (x Data..:? "largeVolumeConf")
            Prelude.<*> (x Data..:? "launchDisposition")
            Prelude.<*> (x Data..:? "licensing")
            Prelude.<*> (x Data..:? "mapAutoTaggingMpeID")
            Prelude.<*> (x Data..:? "postLaunchActions")
            Prelude.<*> (x Data..:? "smallVolumeConf")
            Prelude.<*> (x Data..:? "smallVolumeMaxSize")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targetInstanceTypeRightSizingMethod")
            Prelude.<*> (x Data..: "launchConfigurationTemplateID")
      )

instance Prelude.Hashable LaunchConfigurationTemplate where
  hashWithSalt _salt LaunchConfigurationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` associatePublicIpAddress
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` ec2LaunchTemplateID
      `Prelude.hashWithSalt` enableMapAutoTagging
      `Prelude.hashWithSalt` largeVolumeConf
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` mapAutoTaggingMpeID
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` smallVolumeConf
      `Prelude.hashWithSalt` smallVolumeMaxSize
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
      `Prelude.hashWithSalt` launchConfigurationTemplateID

instance Prelude.NFData LaunchConfigurationTemplate where
  rnf LaunchConfigurationTemplate' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf ec2LaunchTemplateID
      `Prelude.seq` Prelude.rnf enableMapAutoTagging
      `Prelude.seq` Prelude.rnf largeVolumeConf
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf mapAutoTaggingMpeID
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf smallVolumeConf
      `Prelude.seq` Prelude.rnf smallVolumeMaxSize
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf
        launchConfigurationTemplateID
