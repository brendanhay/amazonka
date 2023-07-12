{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.CreateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Launch Configuration Template.
module Amazonka.MGN.CreateLaunchConfigurationTemplate
  ( -- * Creating a Request
    CreateLaunchConfigurationTemplate (..),
    newCreateLaunchConfigurationTemplate,

    -- * Request Lenses
    createLaunchConfigurationTemplate_associatePublicIpAddress,
    createLaunchConfigurationTemplate_bootMode,
    createLaunchConfigurationTemplate_copyPrivateIp,
    createLaunchConfigurationTemplate_copyTags,
    createLaunchConfigurationTemplate_enableMapAutoTagging,
    createLaunchConfigurationTemplate_largeVolumeConf,
    createLaunchConfigurationTemplate_launchDisposition,
    createLaunchConfigurationTemplate_licensing,
    createLaunchConfigurationTemplate_mapAutoTaggingMpeID,
    createLaunchConfigurationTemplate_postLaunchActions,
    createLaunchConfigurationTemplate_smallVolumeConf,
    createLaunchConfigurationTemplate_smallVolumeMaxSize,
    createLaunchConfigurationTemplate_tags,
    createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,

    -- * Destructuring the Response
    LaunchConfigurationTemplate (..),
    newLaunchConfigurationTemplate,

    -- * Response Lenses
    launchConfigurationTemplate_arn,
    launchConfigurationTemplate_associatePublicIpAddress,
    launchConfigurationTemplate_bootMode,
    launchConfigurationTemplate_copyPrivateIp,
    launchConfigurationTemplate_copyTags,
    launchConfigurationTemplate_ec2LaunchTemplateID,
    launchConfigurationTemplate_enableMapAutoTagging,
    launchConfigurationTemplate_largeVolumeConf,
    launchConfigurationTemplate_launchDisposition,
    launchConfigurationTemplate_licensing,
    launchConfigurationTemplate_mapAutoTaggingMpeID,
    launchConfigurationTemplate_postLaunchActions,
    launchConfigurationTemplate_smallVolumeConf,
    launchConfigurationTemplate_smallVolumeMaxSize,
    launchConfigurationTemplate_tags,
    launchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    launchConfigurationTemplate_launchConfigurationTemplateID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchConfigurationTemplate' smart constructor.
data CreateLaunchConfigurationTemplate = CreateLaunchConfigurationTemplate'
  { -- | Associate public Ip address.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Launch configuration template boot mode.
    bootMode :: Prelude.Maybe BootMode,
    -- | Copy private Ip.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Copy tags.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Enable map auto tagging.
    enableMapAutoTagging :: Prelude.Maybe Prelude.Bool,
    -- | Large volume config.
    largeVolumeConf :: Prelude.Maybe LaunchTemplateDiskConf,
    -- | Launch disposition.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    licensing :: Prelude.Maybe Licensing,
    -- | Launch configuration template map auto tagging MPE ID.
    mapAutoTaggingMpeID :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration template post launch actions.
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Small volume config.
    smallVolumeConf :: Prelude.Maybe LaunchTemplateDiskConf,
    -- | Small volume maximum size.
    smallVolumeMaxSize :: Prelude.Maybe Prelude.Natural,
    -- | Request to associate tags during creation of a Launch Configuration
    -- Template.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatePublicIpAddress', 'createLaunchConfigurationTemplate_associatePublicIpAddress' - Associate public Ip address.
--
-- 'bootMode', 'createLaunchConfigurationTemplate_bootMode' - Launch configuration template boot mode.
--
-- 'copyPrivateIp', 'createLaunchConfigurationTemplate_copyPrivateIp' - Copy private Ip.
--
-- 'copyTags', 'createLaunchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'enableMapAutoTagging', 'createLaunchConfigurationTemplate_enableMapAutoTagging' - Enable map auto tagging.
--
-- 'largeVolumeConf', 'createLaunchConfigurationTemplate_largeVolumeConf' - Large volume config.
--
-- 'launchDisposition', 'createLaunchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'createLaunchConfigurationTemplate_licensing' - Undocumented member.
--
-- 'mapAutoTaggingMpeID', 'createLaunchConfigurationTemplate_mapAutoTaggingMpeID' - Launch configuration template map auto tagging MPE ID.
--
-- 'postLaunchActions', 'createLaunchConfigurationTemplate_postLaunchActions' - Launch configuration template post launch actions.
--
-- 'smallVolumeConf', 'createLaunchConfigurationTemplate_smallVolumeConf' - Small volume config.
--
-- 'smallVolumeMaxSize', 'createLaunchConfigurationTemplate_smallVolumeMaxSize' - Small volume maximum size.
--
-- 'tags', 'createLaunchConfigurationTemplate_tags' - Request to associate tags during creation of a Launch Configuration
-- Template.
--
-- 'targetInstanceTypeRightSizingMethod', 'createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
newCreateLaunchConfigurationTemplate ::
  CreateLaunchConfigurationTemplate
newCreateLaunchConfigurationTemplate =
  CreateLaunchConfigurationTemplate'
    { associatePublicIpAddress =
        Prelude.Nothing,
      bootMode = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      copyTags = Prelude.Nothing,
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
        Prelude.Nothing
    }

-- | Associate public Ip address.
createLaunchConfigurationTemplate_associatePublicIpAddress :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_associatePublicIpAddress = Lens.lens (\CreateLaunchConfigurationTemplate' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@CreateLaunchConfigurationTemplate' {} a -> s {associatePublicIpAddress = a} :: CreateLaunchConfigurationTemplate)

-- | Launch configuration template boot mode.
createLaunchConfigurationTemplate_bootMode :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe BootMode)
createLaunchConfigurationTemplate_bootMode = Lens.lens (\CreateLaunchConfigurationTemplate' {bootMode} -> bootMode) (\s@CreateLaunchConfigurationTemplate' {} a -> s {bootMode = a} :: CreateLaunchConfigurationTemplate)

-- | Copy private Ip.
createLaunchConfigurationTemplate_copyPrivateIp :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_copyPrivateIp = Lens.lens (\CreateLaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@CreateLaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: CreateLaunchConfigurationTemplate)

-- | Copy tags.
createLaunchConfigurationTemplate_copyTags :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_copyTags = Lens.lens (\CreateLaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@CreateLaunchConfigurationTemplate' {} a -> s {copyTags = a} :: CreateLaunchConfigurationTemplate)

-- | Enable map auto tagging.
createLaunchConfigurationTemplate_enableMapAutoTagging :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_enableMapAutoTagging = Lens.lens (\CreateLaunchConfigurationTemplate' {enableMapAutoTagging} -> enableMapAutoTagging) (\s@CreateLaunchConfigurationTemplate' {} a -> s {enableMapAutoTagging = a} :: CreateLaunchConfigurationTemplate)

-- | Large volume config.
createLaunchConfigurationTemplate_largeVolumeConf :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
createLaunchConfigurationTemplate_largeVolumeConf = Lens.lens (\CreateLaunchConfigurationTemplate' {largeVolumeConf} -> largeVolumeConf) (\s@CreateLaunchConfigurationTemplate' {} a -> s {largeVolumeConf = a} :: CreateLaunchConfigurationTemplate)

-- | Launch disposition.
createLaunchConfigurationTemplate_launchDisposition :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
createLaunchConfigurationTemplate_launchDisposition = Lens.lens (\CreateLaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@CreateLaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: CreateLaunchConfigurationTemplate)

-- | Undocumented member.
createLaunchConfigurationTemplate_licensing :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Licensing)
createLaunchConfigurationTemplate_licensing = Lens.lens (\CreateLaunchConfigurationTemplate' {licensing} -> licensing) (\s@CreateLaunchConfigurationTemplate' {} a -> s {licensing = a} :: CreateLaunchConfigurationTemplate)

-- | Launch configuration template map auto tagging MPE ID.
createLaunchConfigurationTemplate_mapAutoTaggingMpeID :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
createLaunchConfigurationTemplate_mapAutoTaggingMpeID = Lens.lens (\CreateLaunchConfigurationTemplate' {mapAutoTaggingMpeID} -> mapAutoTaggingMpeID) (\s@CreateLaunchConfigurationTemplate' {} a -> s {mapAutoTaggingMpeID = a} :: CreateLaunchConfigurationTemplate)

-- | Launch configuration template post launch actions.
createLaunchConfigurationTemplate_postLaunchActions :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
createLaunchConfigurationTemplate_postLaunchActions = Lens.lens (\CreateLaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@CreateLaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: CreateLaunchConfigurationTemplate)

-- | Small volume config.
createLaunchConfigurationTemplate_smallVolumeConf :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
createLaunchConfigurationTemplate_smallVolumeConf = Lens.lens (\CreateLaunchConfigurationTemplate' {smallVolumeConf} -> smallVolumeConf) (\s@CreateLaunchConfigurationTemplate' {} a -> s {smallVolumeConf = a} :: CreateLaunchConfigurationTemplate)

-- | Small volume maximum size.
createLaunchConfigurationTemplate_smallVolumeMaxSize :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Natural)
createLaunchConfigurationTemplate_smallVolumeMaxSize = Lens.lens (\CreateLaunchConfigurationTemplate' {smallVolumeMaxSize} -> smallVolumeMaxSize) (\s@CreateLaunchConfigurationTemplate' {} a -> s {smallVolumeMaxSize = a} :: CreateLaunchConfigurationTemplate)

-- | Request to associate tags during creation of a Launch Configuration
-- Template.
createLaunchConfigurationTemplate_tags :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunchConfigurationTemplate_tags = Lens.lens (\CreateLaunchConfigurationTemplate' {tags} -> tags) (\s@CreateLaunchConfigurationTemplate' {} a -> s {tags = a} :: CreateLaunchConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Target instance type right-sizing method.
createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\CreateLaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@CreateLaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: CreateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    CreateLaunchConfigurationTemplate
  where
  type
    AWSResponse CreateLaunchConfigurationTemplate =
      LaunchConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    CreateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    CreateLaunchConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` bootMode
        `Prelude.hashWithSalt` copyPrivateIp
        `Prelude.hashWithSalt` copyTags
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

instance
  Prelude.NFData
    CreateLaunchConfigurationTemplate
  where
  rnf CreateLaunchConfigurationTemplate' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
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

instance
  Data.ToHeaders
    CreateLaunchConfigurationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateLaunchConfigurationTemplate
  where
  toJSON CreateLaunchConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associatePublicIpAddress" Data..=)
              Prelude.<$> associatePublicIpAddress,
            ("bootMode" Data..=) Prelude.<$> bootMode,
            ("copyPrivateIp" Data..=) Prelude.<$> copyPrivateIp,
            ("copyTags" Data..=) Prelude.<$> copyTags,
            ("enableMapAutoTagging" Data..=)
              Prelude.<$> enableMapAutoTagging,
            ("largeVolumeConf" Data..=)
              Prelude.<$> largeVolumeConf,
            ("launchDisposition" Data..=)
              Prelude.<$> launchDisposition,
            ("licensing" Data..=) Prelude.<$> licensing,
            ("mapAutoTaggingMpeID" Data..=)
              Prelude.<$> mapAutoTaggingMpeID,
            ("postLaunchActions" Data..=)
              Prelude.<$> postLaunchActions,
            ("smallVolumeConf" Data..=)
              Prelude.<$> smallVolumeConf,
            ("smallVolumeMaxSize" Data..=)
              Prelude.<$> smallVolumeMaxSize,
            ("tags" Data..=) Prelude.<$> tags,
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod
          ]
      )

instance
  Data.ToPath
    CreateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/CreateLaunchConfigurationTemplate"

instance
  Data.ToQuery
    CreateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
