{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.PutBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Amazon EMR block public access configuration for
-- your AWS account in the current Region. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR>
-- in the /Amazon EMR Management Guide/.
module Network.AWS.EMR.PutBlockPublicAccessConfiguration
  ( -- * Creating a Request
    PutBlockPublicAccessConfiguration (..),
    newPutBlockPublicAccessConfiguration,

    -- * Request Lenses
    putBlockPublicAccessConfiguration_blockPublicAccessConfiguration,

    -- * Destructuring the Response
    PutBlockPublicAccessConfigurationResponse (..),
    newPutBlockPublicAccessConfigurationResponse,

    -- * Response Lenses
    putBlockPublicAccessConfigurationResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutBlockPublicAccessConfiguration' smart constructor.
data PutBlockPublicAccessConfiguration = PutBlockPublicAccessConfiguration'
  { -- | A configuration for Amazon EMR block public access. The configuration
    -- applies to all clusters created in your account for the current Region.
    -- The configuration specifies whether block public access is enabled. If
    -- block public access is enabled, security groups associated with the
    -- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
    -- ::\/0 on a port, unless the port is specified as an exception using
    -- @PermittedPublicSecurityGroupRuleRanges@ in the
    -- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
    -- exception, and public access is allowed on this port. You can change
    -- this by updating @BlockPublicSecurityGroupRules@ to remove the
    -- exception.
    --
    -- For accounts that created clusters in a Region before November 25, 2019,
    -- block public access is disabled by default in that Region. To use this
    -- feature, you must manually enable and configure it. For accounts that
    -- did not create an EMR cluster in a Region before this date, block public
    -- access is enabled by default in that Region.
    blockPublicAccessConfiguration :: BlockPublicAccessConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBlockPublicAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicAccessConfiguration', 'putBlockPublicAccessConfiguration_blockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration
-- applies to all clusters created in your account for the current Region.
-- The configuration specifies whether block public access is enabled. If
-- block public access is enabled, security groups associated with the
-- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
-- ::\/0 on a port, unless the port is specified as an exception using
-- @PermittedPublicSecurityGroupRuleRanges@ in the
-- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
-- exception, and public access is allowed on this port. You can change
-- this by updating @BlockPublicSecurityGroupRules@ to remove the
-- exception.
--
-- For accounts that created clusters in a Region before November 25, 2019,
-- block public access is disabled by default in that Region. To use this
-- feature, you must manually enable and configure it. For accounts that
-- did not create an EMR cluster in a Region before this date, block public
-- access is enabled by default in that Region.
newPutBlockPublicAccessConfiguration ::
  -- | 'blockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  PutBlockPublicAccessConfiguration
newPutBlockPublicAccessConfiguration
  pBlockPublicAccessConfiguration_ =
    PutBlockPublicAccessConfiguration'
      { blockPublicAccessConfiguration =
          pBlockPublicAccessConfiguration_
      }

-- | A configuration for Amazon EMR block public access. The configuration
-- applies to all clusters created in your account for the current Region.
-- The configuration specifies whether block public access is enabled. If
-- block public access is enabled, security groups associated with the
-- cluster cannot have rules that allow inbound traffic from 0.0.0.0\/0 or
-- ::\/0 on a port, unless the port is specified as an exception using
-- @PermittedPublicSecurityGroupRuleRanges@ in the
-- @BlockPublicAccessConfiguration@. By default, Port 22 (SSH) is an
-- exception, and public access is allowed on this port. You can change
-- this by updating @BlockPublicSecurityGroupRules@ to remove the
-- exception.
--
-- For accounts that created clusters in a Region before November 25, 2019,
-- block public access is disabled by default in that Region. To use this
-- feature, you must manually enable and configure it. For accounts that
-- did not create an EMR cluster in a Region before this date, block public
-- access is enabled by default in that Region.
putBlockPublicAccessConfiguration_blockPublicAccessConfiguration :: Lens.Lens' PutBlockPublicAccessConfiguration BlockPublicAccessConfiguration
putBlockPublicAccessConfiguration_blockPublicAccessConfiguration = Lens.lens (\PutBlockPublicAccessConfiguration' {blockPublicAccessConfiguration} -> blockPublicAccessConfiguration) (\s@PutBlockPublicAccessConfiguration' {} a -> s {blockPublicAccessConfiguration = a} :: PutBlockPublicAccessConfiguration)

instance
  Prelude.AWSRequest
    PutBlockPublicAccessConfiguration
  where
  type
    Rs PutBlockPublicAccessConfiguration =
      PutBlockPublicAccessConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutBlockPublicAccessConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutBlockPublicAccessConfiguration

instance
  Prelude.NFData
    PutBlockPublicAccessConfiguration

instance
  Prelude.ToHeaders
    PutBlockPublicAccessConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.PutBlockPublicAccessConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    PutBlockPublicAccessConfiguration
  where
  toJSON PutBlockPublicAccessConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "BlockPublicAccessConfiguration"
                  Prelude..= blockPublicAccessConfiguration
              )
          ]
      )

instance
  Prelude.ToPath
    PutBlockPublicAccessConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    PutBlockPublicAccessConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBlockPublicAccessConfigurationResponse' smart constructor.
data PutBlockPublicAccessConfigurationResponse = PutBlockPublicAccessConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBlockPublicAccessConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putBlockPublicAccessConfigurationResponse_httpStatus' - The response's http status code.
newPutBlockPublicAccessConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutBlockPublicAccessConfigurationResponse
newPutBlockPublicAccessConfigurationResponse
  pHttpStatus_ =
    PutBlockPublicAccessConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putBlockPublicAccessConfigurationResponse_httpStatus :: Lens.Lens' PutBlockPublicAccessConfigurationResponse Prelude.Int
putBlockPublicAccessConfigurationResponse_httpStatus = Lens.lens (\PutBlockPublicAccessConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutBlockPublicAccessConfigurationResponse' {} a -> s {httpStatus = a} :: PutBlockPublicAccessConfigurationResponse)

instance
  Prelude.NFData
    PutBlockPublicAccessConfigurationResponse
