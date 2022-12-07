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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates multiple LaunchConfigurations by Source Server ID.
module Amazonka.MGN.UpdateLaunchConfiguration
  ( -- * Creating a Request
    UpdateLaunchConfiguration (..),
    newUpdateLaunchConfiguration,

    -- * Request Lenses
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_postLaunchActions,
    updateLaunchConfiguration_bootMode,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_sourceServerID,

    -- * Destructuring the Response
    LaunchConfiguration (..),
    newLaunchConfiguration,

    -- * Response Lenses
    launchConfiguration_name,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_copyTags,
    launchConfiguration_launchDisposition,
    launchConfiguration_postLaunchActions,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_bootMode,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,
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
  { -- | Update Launch configuration name request.
    name :: Prelude.Maybe Prelude.Text,
    -- | Update Launch configuration Target instance right sizing request.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Update Launch configuration copy Tags request.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Update Launch configuration launch disposition request.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Update Launch configuration boot mode request.
    bootMode :: Prelude.Maybe BootMode,
    -- | Update Launch configuration licensing request.
    licensing :: Prelude.Maybe Licensing,
    -- | Update Launch configuration copy Private IP request.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
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
-- 'name', 'updateLaunchConfiguration_name' - Update Launch configuration name request.
--
-- 'targetInstanceTypeRightSizingMethod', 'updateLaunchConfiguration_targetInstanceTypeRightSizingMethod' - Update Launch configuration Target instance right sizing request.
--
-- 'copyTags', 'updateLaunchConfiguration_copyTags' - Update Launch configuration copy Tags request.
--
-- 'launchDisposition', 'updateLaunchConfiguration_launchDisposition' - Update Launch configuration launch disposition request.
--
-- 'postLaunchActions', 'updateLaunchConfiguration_postLaunchActions' - Undocumented member.
--
-- 'bootMode', 'updateLaunchConfiguration_bootMode' - Update Launch configuration boot mode request.
--
-- 'licensing', 'updateLaunchConfiguration_licensing' - Update Launch configuration licensing request.
--
-- 'copyPrivateIp', 'updateLaunchConfiguration_copyPrivateIp' - Update Launch configuration copy Private IP request.
--
-- 'sourceServerID', 'updateLaunchConfiguration_sourceServerID' - Update Launch configuration by Source Server ID request.
newUpdateLaunchConfiguration ::
  -- | 'sourceServerID'
  Prelude.Text ->
  UpdateLaunchConfiguration
newUpdateLaunchConfiguration pSourceServerID_ =
  UpdateLaunchConfiguration'
    { name = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing,
      copyTags = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      postLaunchActions = Prelude.Nothing,
      bootMode = Prelude.Nothing,
      licensing = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Update Launch configuration name request.
updateLaunchConfiguration_name :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Text)
updateLaunchConfiguration_name = Lens.lens (\UpdateLaunchConfiguration' {name} -> name) (\s@UpdateLaunchConfiguration' {} a -> s {name = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration Target instance right sizing request.
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\UpdateLaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@UpdateLaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration copy Tags request.
updateLaunchConfiguration_copyTags :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_copyTags = Lens.lens (\UpdateLaunchConfiguration' {copyTags} -> copyTags) (\s@UpdateLaunchConfiguration' {} a -> s {copyTags = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration launch disposition request.
updateLaunchConfiguration_launchDisposition :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe LaunchDisposition)
updateLaunchConfiguration_launchDisposition = Lens.lens (\UpdateLaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@UpdateLaunchConfiguration' {} a -> s {launchDisposition = a} :: UpdateLaunchConfiguration)

-- | Undocumented member.
updateLaunchConfiguration_postLaunchActions :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe PostLaunchActions)
updateLaunchConfiguration_postLaunchActions = Lens.lens (\UpdateLaunchConfiguration' {postLaunchActions} -> postLaunchActions) (\s@UpdateLaunchConfiguration' {} a -> s {postLaunchActions = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration boot mode request.
updateLaunchConfiguration_bootMode :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe BootMode)
updateLaunchConfiguration_bootMode = Lens.lens (\UpdateLaunchConfiguration' {bootMode} -> bootMode) (\s@UpdateLaunchConfiguration' {} a -> s {bootMode = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration licensing request.
updateLaunchConfiguration_licensing :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Licensing)
updateLaunchConfiguration_licensing = Lens.lens (\UpdateLaunchConfiguration' {licensing} -> licensing) (\s@UpdateLaunchConfiguration' {} a -> s {licensing = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration copy Private IP request.
updateLaunchConfiguration_copyPrivateIp :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_copyPrivateIp = Lens.lens (\UpdateLaunchConfiguration' {copyPrivateIp} -> copyPrivateIp) (\s@UpdateLaunchConfiguration' {} a -> s {copyPrivateIp = a} :: UpdateLaunchConfiguration)

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
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData UpdateLaunchConfiguration where
  rnf UpdateLaunchConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf copyPrivateIp
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
          [ ("name" Data..=) Prelude.<$> name,
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod,
            ("copyTags" Data..=) Prelude.<$> copyTags,
            ("launchDisposition" Data..=)
              Prelude.<$> launchDisposition,
            ("postLaunchActions" Data..=)
              Prelude.<$> postLaunchActions,
            ("bootMode" Data..=) Prelude.<$> bootMode,
            ("licensing" Data..=) Prelude.<$> licensing,
            ("copyPrivateIp" Data..=) Prelude.<$> copyPrivateIp,
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath UpdateLaunchConfiguration where
  toPath = Prelude.const "/UpdateLaunchConfiguration"

instance Data.ToQuery UpdateLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty
