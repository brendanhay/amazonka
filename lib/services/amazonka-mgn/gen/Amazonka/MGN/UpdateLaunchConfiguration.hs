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
-- Module      : Amazonka.MGN.UpdateLaunchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates multiple LaunchConfigurations by Source Server ID.
module Amazonka.MGN.UpdateLaunchConfiguration
  ( -- * Creating a Request
    UpdateLaunchConfiguration (..),
    newUpdateLaunchConfiguration,

    -- * Request Lenses
    updateLaunchConfiguration_bootMode,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_enableMapAutoTagging,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_mapAutoTaggingMpeID,
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_postLaunchActions,
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_sourceServerID,

    -- * Destructuring the Response
    LaunchConfiguration (..),
    newLaunchConfiguration,

    -- * Response Lenses
    launchConfiguration_bootMode,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_enableMapAutoTagging,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_mapAutoTaggingMpeID,
    launchConfiguration_name,
    launchConfiguration_postLaunchActions,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLaunchConfiguration' smart constructor.
data UpdateLaunchConfiguration = UpdateLaunchConfiguration'
  { -- | Update Launch configuration boot mode request.
    bootMode :: Prelude.Maybe BootMode,
    -- | Update Launch configuration copy Private IP request.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Update Launch configuration copy Tags request.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Enable map auto tagging.
    enableMapAutoTagging :: Prelude.Maybe Prelude.Bool,
    -- | Update Launch configuration launch disposition request.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Update Launch configuration licensing request.
    licensing :: Prelude.Maybe Licensing,
    -- | Launch configuration map auto tagging MPE ID.
    mapAutoTaggingMpeID :: Prelude.Maybe Prelude.Text,
    -- | Update Launch configuration name request.
    name :: Prelude.Maybe Prelude.Text,
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Update Launch configuration Target instance right sizing request.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Update Launch configuration by Source Server ID request.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bootMode', 'updateLaunchConfiguration_bootMode' - Update Launch configuration boot mode request.
--
-- 'copyPrivateIp', 'updateLaunchConfiguration_copyPrivateIp' - Update Launch configuration copy Private IP request.
--
-- 'copyTags', 'updateLaunchConfiguration_copyTags' - Update Launch configuration copy Tags request.
--
-- 'enableMapAutoTagging', 'updateLaunchConfiguration_enableMapAutoTagging' - Enable map auto tagging.
--
-- 'launchDisposition', 'updateLaunchConfiguration_launchDisposition' - Update Launch configuration launch disposition request.
--
-- 'licensing', 'updateLaunchConfiguration_licensing' - Update Launch configuration licensing request.
--
-- 'mapAutoTaggingMpeID', 'updateLaunchConfiguration_mapAutoTaggingMpeID' - Launch configuration map auto tagging MPE ID.
--
-- 'name', 'updateLaunchConfiguration_name' - Update Launch configuration name request.
--
-- 'postLaunchActions', 'updateLaunchConfiguration_postLaunchActions' - Undocumented member.
--
-- 'targetInstanceTypeRightSizingMethod', 'updateLaunchConfiguration_targetInstanceTypeRightSizingMethod' - Update Launch configuration Target instance right sizing request.
--
-- 'sourceServerID', 'updateLaunchConfiguration_sourceServerID' - Update Launch configuration by Source Server ID request.
newUpdateLaunchConfiguration ::
  -- | 'sourceServerID'
  Prelude.Text ->
  UpdateLaunchConfiguration
newUpdateLaunchConfiguration pSourceServerID_ =
  UpdateLaunchConfiguration'
    { bootMode =
        Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      enableMapAutoTagging = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      licensing = Prelude.Nothing,
      mapAutoTaggingMpeID = Prelude.Nothing,
      name = Prelude.Nothing,
      postLaunchActions = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Update Launch configuration boot mode request.
updateLaunchConfiguration_bootMode :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe BootMode)
updateLaunchConfiguration_bootMode = Lens.lens (\UpdateLaunchConfiguration' {bootMode} -> bootMode) (\s@UpdateLaunchConfiguration' {} a -> s {bootMode = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration copy Private IP request.
updateLaunchConfiguration_copyPrivateIp :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_copyPrivateIp = Lens.lens (\UpdateLaunchConfiguration' {copyPrivateIp} -> copyPrivateIp) (\s@UpdateLaunchConfiguration' {} a -> s {copyPrivateIp = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration copy Tags request.
updateLaunchConfiguration_copyTags :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_copyTags = Lens.lens (\UpdateLaunchConfiguration' {copyTags} -> copyTags) (\s@UpdateLaunchConfiguration' {} a -> s {copyTags = a} :: UpdateLaunchConfiguration)

-- | Enable map auto tagging.
updateLaunchConfiguration_enableMapAutoTagging :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_enableMapAutoTagging = Lens.lens (\UpdateLaunchConfiguration' {enableMapAutoTagging} -> enableMapAutoTagging) (\s@UpdateLaunchConfiguration' {} a -> s {enableMapAutoTagging = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration launch disposition request.
updateLaunchConfiguration_launchDisposition :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe LaunchDisposition)
updateLaunchConfiguration_launchDisposition = Lens.lens (\UpdateLaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@UpdateLaunchConfiguration' {} a -> s {launchDisposition = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration licensing request.
updateLaunchConfiguration_licensing :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Licensing)
updateLaunchConfiguration_licensing = Lens.lens (\UpdateLaunchConfiguration' {licensing} -> licensing) (\s@UpdateLaunchConfiguration' {} a -> s {licensing = a} :: UpdateLaunchConfiguration)

-- | Launch configuration map auto tagging MPE ID.
updateLaunchConfiguration_mapAutoTaggingMpeID :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Text)
updateLaunchConfiguration_mapAutoTaggingMpeID = Lens.lens (\UpdateLaunchConfiguration' {mapAutoTaggingMpeID} -> mapAutoTaggingMpeID) (\s@UpdateLaunchConfiguration' {} a -> s {mapAutoTaggingMpeID = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration name request.
updateLaunchConfiguration_name :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Text)
updateLaunchConfiguration_name = Lens.lens (\UpdateLaunchConfiguration' {name} -> name) (\s@UpdateLaunchConfiguration' {} a -> s {name = a} :: UpdateLaunchConfiguration)

-- | Undocumented member.
updateLaunchConfiguration_postLaunchActions :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe PostLaunchActions)
updateLaunchConfiguration_postLaunchActions = Lens.lens (\UpdateLaunchConfiguration' {postLaunchActions} -> postLaunchActions) (\s@UpdateLaunchConfiguration' {} a -> s {postLaunchActions = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration Target instance right sizing request.
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\UpdateLaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@UpdateLaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration by Source Server ID request.
updateLaunchConfiguration_sourceServerID :: Lens.Lens' UpdateLaunchConfiguration Prelude.Text
updateLaunchConfiguration_sourceServerID = Lens.lens (\UpdateLaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@UpdateLaunchConfiguration' {} a -> s {sourceServerID = a} :: UpdateLaunchConfiguration)

instance Core.AWSRequest UpdateLaunchConfiguration where
  type
    AWSResponse UpdateLaunchConfiguration =
      LaunchConfiguration
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateLaunchConfiguration where
  hashWithSalt _salt UpdateLaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` enableMapAutoTagging
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` mapAutoTaggingMpeID
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData UpdateLaunchConfiguration where
  rnf UpdateLaunchConfiguration' {..} =
    Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf enableMapAutoTagging
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf mapAutoTaggingMpeID
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToHeaders UpdateLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLaunchConfiguration where
  toJSON UpdateLaunchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bootMode" Data..=) Prelude.<$> bootMode,
            ("copyPrivateIp" Data..=) Prelude.<$> copyPrivateIp,
            ("copyTags" Data..=) Prelude.<$> copyTags,
            ("enableMapAutoTagging" Data..=)
              Prelude.<$> enableMapAutoTagging,
            ("launchDisposition" Data..=)
              Prelude.<$> launchDisposition,
            ("licensing" Data..=) Prelude.<$> licensing,
            ("mapAutoTaggingMpeID" Data..=)
              Prelude.<$> mapAutoTaggingMpeID,
            ("name" Data..=) Prelude.<$> name,
            ("postLaunchActions" Data..=)
              Prelude.<$> postLaunchActions,
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod,
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath UpdateLaunchConfiguration where
  toPath = Prelude.const "/UpdateLaunchConfiguration"

instance Data.ToQuery UpdateLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty
