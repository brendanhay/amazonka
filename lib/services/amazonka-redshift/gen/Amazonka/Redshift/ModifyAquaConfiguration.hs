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
-- Module      : Amazonka.Redshift.ModifyAquaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is retired. Calling this operation does not change AQUA
-- configuration. Amazon Redshift automatically determines whether to use
-- AQUA (Advanced Query Accelerator).
module Amazonka.Redshift.ModifyAquaConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyAquaConfiguration' smart constructor.
data ModifyAquaConfiguration = ModifyAquaConfiguration'
  { -- | This parameter is retired. Amazon Redshift automatically determines
    -- whether to use AQUA (Advanced Query Accelerator).
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
-- 'aquaConfigurationStatus', 'modifyAquaConfiguration_aquaConfigurationStatus' - This parameter is retired. Amazon Redshift automatically determines
-- whether to use AQUA (Advanced Query Accelerator).
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

-- | This parameter is retired. Amazon Redshift automatically determines
-- whether to use AQUA (Advanced Query Accelerator).
modifyAquaConfiguration_aquaConfigurationStatus :: Lens.Lens' ModifyAquaConfiguration (Prelude.Maybe AquaConfigurationStatus)
modifyAquaConfiguration_aquaConfigurationStatus = Lens.lens (\ModifyAquaConfiguration' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@ModifyAquaConfiguration' {} a -> s {aquaConfigurationStatus = a} :: ModifyAquaConfiguration)

-- | The identifier of the cluster to be modified.
modifyAquaConfiguration_clusterIdentifier :: Lens.Lens' ModifyAquaConfiguration Prelude.Text
modifyAquaConfiguration_clusterIdentifier = Lens.lens (\ModifyAquaConfiguration' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyAquaConfiguration' {} a -> s {clusterIdentifier = a} :: ModifyAquaConfiguration)

instance Core.AWSRequest ModifyAquaConfiguration where
  type
    AWSResponse ModifyAquaConfiguration =
      ModifyAquaConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyAquaConfigurationResult"
      ( \s h x ->
          ModifyAquaConfigurationResponse'
            Prelude.<$> (x Data..@? "AquaConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAquaConfiguration where
  hashWithSalt _salt ModifyAquaConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` aquaConfigurationStatus
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ModifyAquaConfiguration where
  rnf ModifyAquaConfiguration' {..} =
    Prelude.rnf aquaConfigurationStatus
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders ModifyAquaConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyAquaConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyAquaConfiguration where
  toQuery ModifyAquaConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyAquaConfiguration" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AquaConfigurationStatus"
          Data.=: aquaConfigurationStatus,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyAquaConfigurationResponse' smart constructor.
data ModifyAquaConfigurationResponse = ModifyAquaConfigurationResponse'
  { -- | This parameter is retired. Amazon Redshift automatically determines
    -- whether to use AQUA (Advanced Query Accelerator).
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
-- 'aquaConfiguration', 'modifyAquaConfigurationResponse_aquaConfiguration' - This parameter is retired. Amazon Redshift automatically determines
-- whether to use AQUA (Advanced Query Accelerator).
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

-- | This parameter is retired. Amazon Redshift automatically determines
-- whether to use AQUA (Advanced Query Accelerator).
modifyAquaConfigurationResponse_aquaConfiguration :: Lens.Lens' ModifyAquaConfigurationResponse (Prelude.Maybe AquaConfiguration)
modifyAquaConfigurationResponse_aquaConfiguration = Lens.lens (\ModifyAquaConfigurationResponse' {aquaConfiguration} -> aquaConfiguration) (\s@ModifyAquaConfigurationResponse' {} a -> s {aquaConfiguration = a} :: ModifyAquaConfigurationResponse)

-- | The response's http status code.
modifyAquaConfigurationResponse_httpStatus :: Lens.Lens' ModifyAquaConfigurationResponse Prelude.Int
modifyAquaConfigurationResponse_httpStatus = Lens.lens (\ModifyAquaConfigurationResponse' {httpStatus} -> httpStatus) (\s@ModifyAquaConfigurationResponse' {} a -> s {httpStatus = a} :: ModifyAquaConfigurationResponse)

instance
  Prelude.NFData
    ModifyAquaConfigurationResponse
  where
  rnf ModifyAquaConfigurationResponse' {..} =
    Prelude.rnf aquaConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
