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
-- Module      : Network.AWS.MGN.UpdateLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates multiple LaunchConfigurations by Source Server ID.
module Network.AWS.MGN.UpdateLaunchConfiguration
  ( -- * Creating a Request
    UpdateLaunchConfiguration (..),
    newUpdateLaunchConfiguration,

    -- * Request Lenses
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_sourceServerID,

    -- * Destructuring the Response
    LaunchConfiguration (..),
    newLaunchConfiguration,

    -- * Response Lenses
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_launchDisposition,
    launchConfiguration_copyTags,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLaunchConfiguration' smart constructor.
data UpdateLaunchConfiguration = UpdateLaunchConfiguration'
  { -- | Update Launch configuration Target instance right sizing request.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Update Launch configuration launch disposition request.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Update Launch configuration copy Tags request.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Update Launch configuration name request.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'targetInstanceTypeRightSizingMethod', 'updateLaunchConfiguration_targetInstanceTypeRightSizingMethod' - Update Launch configuration Target instance right sizing request.
--
-- 'launchDisposition', 'updateLaunchConfiguration_launchDisposition' - Update Launch configuration launch disposition request.
--
-- 'copyTags', 'updateLaunchConfiguration_copyTags' - Update Launch configuration copy Tags request.
--
-- 'name', 'updateLaunchConfiguration_name' - Update Launch configuration name request.
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
    { targetInstanceTypeRightSizingMethod =
        Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      name = Prelude.Nothing,
      licensing = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Update Launch configuration Target instance right sizing request.
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
updateLaunchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\UpdateLaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@UpdateLaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration launch disposition request.
updateLaunchConfiguration_launchDisposition :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe LaunchDisposition)
updateLaunchConfiguration_launchDisposition = Lens.lens (\UpdateLaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@UpdateLaunchConfiguration' {} a -> s {launchDisposition = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration copy Tags request.
updateLaunchConfiguration_copyTags :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
updateLaunchConfiguration_copyTags = Lens.lens (\UpdateLaunchConfiguration' {copyTags} -> copyTags) (\s@UpdateLaunchConfiguration' {} a -> s {copyTags = a} :: UpdateLaunchConfiguration)

-- | Update Launch configuration name request.
updateLaunchConfiguration_name :: Lens.Lens' UpdateLaunchConfiguration (Prelude.Maybe Prelude.Text)
updateLaunchConfiguration_name = Lens.lens (\UpdateLaunchConfiguration' {name} -> name) (\s@UpdateLaunchConfiguration' {} a -> s {name = a} :: UpdateLaunchConfiguration)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateLaunchConfiguration

instance Prelude.NFData UpdateLaunchConfiguration

instance Core.ToHeaders UpdateLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLaunchConfiguration where
  toJSON UpdateLaunchConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetInstanceTypeRightSizingMethod" Core..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod,
            ("launchDisposition" Core..=)
              Prelude.<$> launchDisposition,
            ("copyTags" Core..=) Prelude.<$> copyTags,
            ("name" Core..=) Prelude.<$> name,
            ("licensing" Core..=) Prelude.<$> licensing,
            ("copyPrivateIp" Core..=) Prelude.<$> copyPrivateIp,
            Prelude.Just
              ("sourceServerID" Core..= sourceServerID)
          ]
      )

instance Core.ToPath UpdateLaunchConfiguration where
  toPath = Prelude.const "/UpdateLaunchConfiguration"

instance Core.ToQuery UpdateLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty
