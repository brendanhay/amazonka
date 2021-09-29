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
-- Module      : Network.AWS.Redshift.ModifyAquaConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies whether a cluster can use AQUA (Advanced Query Accelerator).
module Network.AWS.Redshift.ModifyAquaConfiguration
  ( -- * Creating a Request
    ModifyAquaConfiguration (..),
    newModifyAquaConfiguration,

    -- * Request Lenses
    modifyAquaConfiguration_aquaConfigurationStatus,
    modifyAquaConfiguration_clusterIdentifier,

    -- * Destructuring the Response
    ModifyAquaConfigurationResponse (..),
    newModifyAquaConfigurationResponse,

    -- * Response Lenses
    modifyAquaConfigurationResponse_aquaConfiguration,
    modifyAquaConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyAquaConfiguration' smart constructor.
data ModifyAquaConfiguration = ModifyAquaConfiguration'
  { -- | The new value of AQUA configuration status. Possible values include the
    -- following.
    --
    -- -   enabled - Use AQUA if it is available for the current Amazon Web
    --     Services Region and Amazon Redshift node type.
    --
    -- -   disabled - Don\'t use AQUA.
    --
    -- -   auto - Amazon Redshift determines whether to use AQUA.
    aquaConfigurationStatus :: Prelude.Maybe AquaConfigurationStatus,
    -- | The identifier of the cluster to be modified.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAquaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aquaConfigurationStatus', 'modifyAquaConfiguration_aquaConfigurationStatus' - The new value of AQUA configuration status. Possible values include the
-- following.
--
-- -   enabled - Use AQUA if it is available for the current Amazon Web
--     Services Region and Amazon Redshift node type.
--
-- -   disabled - Don\'t use AQUA.
--
-- -   auto - Amazon Redshift determines whether to use AQUA.
--
-- 'clusterIdentifier', 'modifyAquaConfiguration_clusterIdentifier' - The identifier of the cluster to be modified.
newModifyAquaConfiguration ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyAquaConfiguration
newModifyAquaConfiguration pClusterIdentifier_ =
  ModifyAquaConfiguration'
    { aquaConfigurationStatus =
        Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The new value of AQUA configuration status. Possible values include the
-- following.
--
-- -   enabled - Use AQUA if it is available for the current Amazon Web
--     Services Region and Amazon Redshift node type.
--
-- -   disabled - Don\'t use AQUA.
--
-- -   auto - Amazon Redshift determines whether to use AQUA.
modifyAquaConfiguration_aquaConfigurationStatus :: Lens.Lens' ModifyAquaConfiguration (Prelude.Maybe AquaConfigurationStatus)
modifyAquaConfiguration_aquaConfigurationStatus = Lens.lens (\ModifyAquaConfiguration' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@ModifyAquaConfiguration' {} a -> s {aquaConfigurationStatus = a} :: ModifyAquaConfiguration)

-- | The identifier of the cluster to be modified.
modifyAquaConfiguration_clusterIdentifier :: Lens.Lens' ModifyAquaConfiguration Prelude.Text
modifyAquaConfiguration_clusterIdentifier = Lens.lens (\ModifyAquaConfiguration' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyAquaConfiguration' {} a -> s {clusterIdentifier = a} :: ModifyAquaConfiguration)

instance Core.AWSRequest ModifyAquaConfiguration where
  type
    AWSResponse ModifyAquaConfiguration =
      ModifyAquaConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyAquaConfigurationResult"
      ( \s h x ->
          ModifyAquaConfigurationResponse'
            Prelude.<$> (x Core..@? "AquaConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAquaConfiguration

instance Prelude.NFData ModifyAquaConfiguration

instance Core.ToHeaders ModifyAquaConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyAquaConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyAquaConfiguration where
  toQuery ModifyAquaConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyAquaConfiguration" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AquaConfigurationStatus"
          Core.=: aquaConfigurationStatus,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyAquaConfigurationResponse' smart constructor.
data ModifyAquaConfigurationResponse = ModifyAquaConfigurationResponse'
  { -- | The updated AQUA configuration of the cluster.
    aquaConfiguration :: Prelude.Maybe AquaConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAquaConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aquaConfiguration', 'modifyAquaConfigurationResponse_aquaConfiguration' - The updated AQUA configuration of the cluster.
--
-- 'httpStatus', 'modifyAquaConfigurationResponse_httpStatus' - The response's http status code.
newModifyAquaConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAquaConfigurationResponse
newModifyAquaConfigurationResponse pHttpStatus_ =
  ModifyAquaConfigurationResponse'
    { aquaConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated AQUA configuration of the cluster.
modifyAquaConfigurationResponse_aquaConfiguration :: Lens.Lens' ModifyAquaConfigurationResponse (Prelude.Maybe AquaConfiguration)
modifyAquaConfigurationResponse_aquaConfiguration = Lens.lens (\ModifyAquaConfigurationResponse' {aquaConfiguration} -> aquaConfiguration) (\s@ModifyAquaConfigurationResponse' {} a -> s {aquaConfiguration = a} :: ModifyAquaConfigurationResponse)

-- | The response's http status code.
modifyAquaConfigurationResponse_httpStatus :: Lens.Lens' ModifyAquaConfigurationResponse Prelude.Int
modifyAquaConfigurationResponse_httpStatus = Lens.lens (\ModifyAquaConfigurationResponse' {httpStatus} -> httpStatus) (\s@ModifyAquaConfigurationResponse' {} a -> s {httpStatus = a} :: ModifyAquaConfigurationResponse)

instance
  Prelude.NFData
    ModifyAquaConfigurationResponse
