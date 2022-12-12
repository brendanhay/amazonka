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
-- Module      : Amazonka.MGN.UpdateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Launch Configuration Template by ID.
module Amazonka.MGN.UpdateLaunchConfigurationTemplate
  ( -- * Creating a Request
    UpdateLaunchConfigurationTemplate (..),
    newUpdateLaunchConfigurationTemplate,

    -- * Request Lenses
    updateLaunchConfigurationTemplate_associatePublicIpAddress,
    updateLaunchConfigurationTemplate_bootMode,
    updateLaunchConfigurationTemplate_copyPrivateIp,
    updateLaunchConfigurationTemplate_copyTags,
    updateLaunchConfigurationTemplate_enableMapAutoTagging,
    updateLaunchConfigurationTemplate_largeVolumeConf,
    updateLaunchConfigurationTemplate_launchDisposition,
    updateLaunchConfigurationTemplate_licensing,
    updateLaunchConfigurationTemplate_mapAutoTaggingMpeID,
    updateLaunchConfigurationTemplate_postLaunchActions,
    updateLaunchConfigurationTemplate_smallVolumeConf,
    updateLaunchConfigurationTemplate_smallVolumeMaxSize,
    updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    updateLaunchConfigurationTemplate_launchConfigurationTemplateID,

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

-- | /See:/ 'newUpdateLaunchConfigurationTemplate' smart constructor.
data UpdateLaunchConfigurationTemplate = UpdateLaunchConfigurationTemplate'
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
    -- | Post Launch Action to execute on the Test or Cutover instance.
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Small volume config.
    smallVolumeConf :: Prelude.Maybe LaunchTemplateDiskConf,
    -- | Small volume maximum size.
    smallVolumeMaxSize :: Prelude.Maybe Prelude.Natural,
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Launch Configuration Template ID.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatePublicIpAddress', 'updateLaunchConfigurationTemplate_associatePublicIpAddress' - Associate public Ip address.
--
-- 'bootMode', 'updateLaunchConfigurationTemplate_bootMode' - Launch configuration template boot mode.
--
-- 'copyPrivateIp', 'updateLaunchConfigurationTemplate_copyPrivateIp' - Copy private Ip.
--
-- 'copyTags', 'updateLaunchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'enableMapAutoTagging', 'updateLaunchConfigurationTemplate_enableMapAutoTagging' - Enable map auto tagging.
--
-- 'largeVolumeConf', 'updateLaunchConfigurationTemplate_largeVolumeConf' - Large volume config.
--
-- 'launchDisposition', 'updateLaunchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'updateLaunchConfigurationTemplate_licensing' - Undocumented member.
--
-- 'mapAutoTaggingMpeID', 'updateLaunchConfigurationTemplate_mapAutoTaggingMpeID' - Launch configuration template map auto tagging MPE ID.
--
-- 'postLaunchActions', 'updateLaunchConfigurationTemplate_postLaunchActions' - Post Launch Action to execute on the Test or Cutover instance.
--
-- 'smallVolumeConf', 'updateLaunchConfigurationTemplate_smallVolumeConf' - Small volume config.
--
-- 'smallVolumeMaxSize', 'updateLaunchConfigurationTemplate_smallVolumeMaxSize' - Small volume maximum size.
--
-- 'targetInstanceTypeRightSizingMethod', 'updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
--
-- 'launchConfigurationTemplateID', 'updateLaunchConfigurationTemplate_launchConfigurationTemplateID' - Launch Configuration Template ID.
newUpdateLaunchConfigurationTemplate ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  UpdateLaunchConfigurationTemplate
newUpdateLaunchConfigurationTemplate
  pLaunchConfigurationTemplateID_ =
    UpdateLaunchConfigurationTemplate'
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
        targetInstanceTypeRightSizingMethod =
          Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Associate public Ip address.
updateLaunchConfigurationTemplate_associatePublicIpAddress :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_associatePublicIpAddress = Lens.lens (\UpdateLaunchConfigurationTemplate' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {associatePublicIpAddress = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch configuration template boot mode.
updateLaunchConfigurationTemplate_bootMode :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe BootMode)
updateLaunchConfigurationTemplate_bootMode = Lens.lens (\UpdateLaunchConfigurationTemplate' {bootMode} -> bootMode) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {bootMode = a} :: UpdateLaunchConfigurationTemplate)

-- | Copy private Ip.
updateLaunchConfigurationTemplate_copyPrivateIp :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_copyPrivateIp = Lens.lens (\UpdateLaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: UpdateLaunchConfigurationTemplate)

-- | Copy tags.
updateLaunchConfigurationTemplate_copyTags :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_copyTags = Lens.lens (\UpdateLaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {copyTags = a} :: UpdateLaunchConfigurationTemplate)

-- | Enable map auto tagging.
updateLaunchConfigurationTemplate_enableMapAutoTagging :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_enableMapAutoTagging = Lens.lens (\UpdateLaunchConfigurationTemplate' {enableMapAutoTagging} -> enableMapAutoTagging) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {enableMapAutoTagging = a} :: UpdateLaunchConfigurationTemplate)

-- | Large volume config.
updateLaunchConfigurationTemplate_largeVolumeConf :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
updateLaunchConfigurationTemplate_largeVolumeConf = Lens.lens (\UpdateLaunchConfigurationTemplate' {largeVolumeConf} -> largeVolumeConf) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {largeVolumeConf = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch disposition.
updateLaunchConfigurationTemplate_launchDisposition :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
updateLaunchConfigurationTemplate_launchDisposition = Lens.lens (\UpdateLaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: UpdateLaunchConfigurationTemplate)

-- | Undocumented member.
updateLaunchConfigurationTemplate_licensing :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Licensing)
updateLaunchConfigurationTemplate_licensing = Lens.lens (\UpdateLaunchConfigurationTemplate' {licensing} -> licensing) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {licensing = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch configuration template map auto tagging MPE ID.
updateLaunchConfigurationTemplate_mapAutoTaggingMpeID :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateLaunchConfigurationTemplate_mapAutoTaggingMpeID = Lens.lens (\UpdateLaunchConfigurationTemplate' {mapAutoTaggingMpeID} -> mapAutoTaggingMpeID) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {mapAutoTaggingMpeID = a} :: UpdateLaunchConfigurationTemplate)

-- | Post Launch Action to execute on the Test or Cutover instance.
updateLaunchConfigurationTemplate_postLaunchActions :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
updateLaunchConfigurationTemplate_postLaunchActions = Lens.lens (\UpdateLaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: UpdateLaunchConfigurationTemplate)

-- | Small volume config.
updateLaunchConfigurationTemplate_smallVolumeConf :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe LaunchTemplateDiskConf)
updateLaunchConfigurationTemplate_smallVolumeConf = Lens.lens (\UpdateLaunchConfigurationTemplate' {smallVolumeConf} -> smallVolumeConf) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {smallVolumeConf = a} :: UpdateLaunchConfigurationTemplate)

-- | Small volume maximum size.
updateLaunchConfigurationTemplate_smallVolumeMaxSize :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Natural)
updateLaunchConfigurationTemplate_smallVolumeMaxSize = Lens.lens (\UpdateLaunchConfigurationTemplate' {smallVolumeMaxSize} -> smallVolumeMaxSize) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {smallVolumeMaxSize = a} :: UpdateLaunchConfigurationTemplate)

-- | Target instance type right-sizing method.
updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\UpdateLaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch Configuration Template ID.
updateLaunchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' UpdateLaunchConfigurationTemplate Prelude.Text
updateLaunchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\UpdateLaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: UpdateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    UpdateLaunchConfigurationTemplate
  where
  type
    AWSResponse UpdateLaunchConfigurationTemplate =
      LaunchConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    UpdateLaunchConfigurationTemplate' {..} =
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
        `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
        `Prelude.hashWithSalt` launchConfigurationTemplateID

instance
  Prelude.NFData
    UpdateLaunchConfigurationTemplate
  where
  rnf UpdateLaunchConfigurationTemplate' {..} =
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
      `Prelude.seq` Prelude.rnf
        targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID

instance
  Data.ToHeaders
    UpdateLaunchConfigurationTemplate
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
    UpdateLaunchConfigurationTemplate
  where
  toJSON UpdateLaunchConfigurationTemplate' {..} =
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
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod,
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              )
          ]
      )

instance
  Data.ToPath
    UpdateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/UpdateLaunchConfigurationTemplate"

instance
  Data.ToQuery
    UpdateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
