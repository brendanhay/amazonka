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
-- Module      : Amazonka.EMR.GetBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Amazon EMR block public access configuration for your Amazon
-- Web Services account in the current Region. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR>
-- in the /Amazon EMR Management Guide/.
module Amazonka.EMR.GetBlockPublicAccessConfiguration
  ( -- * Creating a Request
    GetBlockPublicAccessConfiguration (..),
    newGetBlockPublicAccessConfiguration,

    -- * Destructuring the Response
    GetBlockPublicAccessConfigurationResponse (..),
    newGetBlockPublicAccessConfigurationResponse,

    -- * Response Lenses
    getBlockPublicAccessConfigurationResponse_httpStatus,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBlockPublicAccessConfiguration' smart constructor.
data GetBlockPublicAccessConfiguration = GetBlockPublicAccessConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlockPublicAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetBlockPublicAccessConfiguration ::
  GetBlockPublicAccessConfiguration
newGetBlockPublicAccessConfiguration =
  GetBlockPublicAccessConfiguration'

instance
  Core.AWSRequest
    GetBlockPublicAccessConfiguration
  where
  type
    AWSResponse GetBlockPublicAccessConfiguration =
      GetBlockPublicAccessConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlockPublicAccessConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "BlockPublicAccessConfiguration")
              Prelude.<*> ( x
                              Data..:> "BlockPublicAccessConfigurationMetadata"
                          )
      )

instance
  Prelude.Hashable
    GetBlockPublicAccessConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetBlockPublicAccessConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetBlockPublicAccessConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.GetBlockPublicAccessConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetBlockPublicAccessConfiguration
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    GetBlockPublicAccessConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetBlockPublicAccessConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlockPublicAccessConfigurationResponse' smart constructor.
data GetBlockPublicAccessConfigurationResponse = GetBlockPublicAccessConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A configuration for Amazon EMR block public access. The configuration
    -- applies to all clusters created in your account for the current Region.
    -- The configuration specifies whether block public access is enabled. If
    -- block public access is enabled, security groups associated with the
    -- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
    -- ::\/0 on a port, unless the port is specified as an exception using
    -- @PermittedPublicSecurityGroupRuleRanges@ in the
    -- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
    -- exception, and public access is allowed on this port. You can change
    -- this by updating the block public access configuration to remove the
    -- exception.
    --
    -- For accounts that created clusters in a Region before November 25, 2019,
    -- block public access is disabled by default in that Region. To use this
    -- feature, you must manually enable and configure it. For accounts that
    -- did not create an EMR cluster in a Region before this date, block public
    -- access is enabled by default in that Region.
    blockPublicAccessConfiguration :: BlockPublicAccessConfiguration,
    -- | Properties that describe the Amazon Web Services principal that created
    -- the @BlockPublicAccessConfiguration@ using the
    -- @PutBlockPublicAccessConfiguration@ action as well as the date and time
    -- that the configuration was created. Each time a configuration for block
    -- public access is updated, Amazon EMR updates this metadata.
    blockPublicAccessConfigurationMetadata :: BlockPublicAccessConfigurationMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlockPublicAccessConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBlockPublicAccessConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'blockPublicAccessConfiguration', 'getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration
-- applies to all clusters created in your account for the current Region.
-- The configuration specifies whether block public access is enabled. If
-- block public access is enabled, security groups associated with the
-- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
-- ::\/0 on a port, unless the port is specified as an exception using
-- @PermittedPublicSecurityGroupRuleRanges@ in the
-- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
-- exception, and public access is allowed on this port. You can change
-- this by updating the block public access configuration to remove the
-- exception.
--
-- For accounts that created clusters in a Region before November 25, 2019,
-- block public access is disabled by default in that Region. To use this
-- feature, you must manually enable and configure it. For accounts that
-- did not create an EMR cluster in a Region before this date, block public
-- access is enabled by default in that Region.
--
-- 'blockPublicAccessConfigurationMetadata', 'getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata' - Properties that describe the Amazon Web Services principal that created
-- the @BlockPublicAccessConfiguration@ using the
-- @PutBlockPublicAccessConfiguration@ action as well as the date and time
-- that the configuration was created. Each time a configuration for block
-- public access is updated, Amazon EMR updates this metadata.
newGetBlockPublicAccessConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'blockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  -- | 'blockPublicAccessConfigurationMetadata'
  BlockPublicAccessConfigurationMetadata ->
  GetBlockPublicAccessConfigurationResponse
newGetBlockPublicAccessConfigurationResponse
  pHttpStatus_
  pBlockPublicAccessConfiguration_
  pBlockPublicAccessConfigurationMetadata_ =
    GetBlockPublicAccessConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        blockPublicAccessConfiguration =
          pBlockPublicAccessConfiguration_,
        blockPublicAccessConfigurationMetadata =
          pBlockPublicAccessConfigurationMetadata_
      }

-- | The response's http status code.
getBlockPublicAccessConfigurationResponse_httpStatus :: Lens.Lens' GetBlockPublicAccessConfigurationResponse Prelude.Int
getBlockPublicAccessConfigurationResponse_httpStatus = Lens.lens (\GetBlockPublicAccessConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBlockPublicAccessConfigurationResponse' {} a -> s {httpStatus = a} :: GetBlockPublicAccessConfigurationResponse)

-- | A configuration for Amazon EMR block public access. The configuration
-- applies to all clusters created in your account for the current Region.
-- The configuration specifies whether block public access is enabled. If
-- block public access is enabled, security groups associated with the
-- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
-- ::\/0 on a port, unless the port is specified as an exception using
-- @PermittedPublicSecurityGroupRuleRanges@ in the
-- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
-- exception, and public access is allowed on this port. You can change
-- this by updating the block public access configuration to remove the
-- exception.
--
-- For accounts that created clusters in a Region before November 25, 2019,
-- block public access is disabled by default in that Region. To use this
-- feature, you must manually enable and configure it. For accounts that
-- did not create an EMR cluster in a Region before this date, block public
-- access is enabled by default in that Region.
getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration :: Lens.Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfiguration
getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration = Lens.lens (\GetBlockPublicAccessConfigurationResponse' {blockPublicAccessConfiguration} -> blockPublicAccessConfiguration) (\s@GetBlockPublicAccessConfigurationResponse' {} a -> s {blockPublicAccessConfiguration = a} :: GetBlockPublicAccessConfigurationResponse)

-- | Properties that describe the Amazon Web Services principal that created
-- the @BlockPublicAccessConfiguration@ using the
-- @PutBlockPublicAccessConfiguration@ action as well as the date and time
-- that the configuration was created. Each time a configuration for block
-- public access is updated, Amazon EMR updates this metadata.
getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata :: Lens.Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfigurationMetadata
getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata = Lens.lens (\GetBlockPublicAccessConfigurationResponse' {blockPublicAccessConfigurationMetadata} -> blockPublicAccessConfigurationMetadata) (\s@GetBlockPublicAccessConfigurationResponse' {} a -> s {blockPublicAccessConfigurationMetadata = a} :: GetBlockPublicAccessConfigurationResponse)

instance
  Prelude.NFData
    GetBlockPublicAccessConfigurationResponse
  where
  rnf GetBlockPublicAccessConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf blockPublicAccessConfiguration
      `Prelude.seq` Prelude.rnf blockPublicAccessConfigurationMetadata
