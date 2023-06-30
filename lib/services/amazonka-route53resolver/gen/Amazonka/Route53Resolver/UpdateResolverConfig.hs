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
-- Module      : Amazonka.Route53Resolver.UpdateResolverConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the behavior configuration of Route 53 Resolver behavior for a
-- single VPC from Amazon Virtual Private Cloud.
module Amazonka.Route53Resolver.UpdateResolverConfig
  ( -- * Creating a Request
    UpdateResolverConfig (..),
    newUpdateResolverConfig,

    -- * Request Lenses
    updateResolverConfig_resourceId,
    updateResolverConfig_autodefinedReverseFlag,

    -- * Destructuring the Response
    UpdateResolverConfigResponse (..),
    newUpdateResolverConfigResponse,

    -- * Response Lenses
    updateResolverConfigResponse_resolverConfig,
    updateResolverConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newUpdateResolverConfig' smart constructor.
data UpdateResolverConfig = UpdateResolverConfig'
  { -- | Resource ID of the Amazon VPC that you want to update the Resolver
    -- configuration for.
    resourceId :: Prelude.Text,
    -- | Indicates whether or not the Resolver will create autodefined rules for
    -- reverse DNS lookups. This is enabled by default. Disabling this option
    -- will also affect EC2-Classic instances using ClassicLink. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
    -- in the /Amazon EC2 guide/.
    --
    -- It can take some time for the status change to be completed.
    autodefinedReverseFlag :: AutodefinedReverseFlag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResolverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'updateResolverConfig_resourceId' - Resource ID of the Amazon VPC that you want to update the Resolver
-- configuration for.
--
-- 'autodefinedReverseFlag', 'updateResolverConfig_autodefinedReverseFlag' - Indicates whether or not the Resolver will create autodefined rules for
-- reverse DNS lookups. This is enabled by default. Disabling this option
-- will also affect EC2-Classic instances using ClassicLink. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 guide/.
--
-- It can take some time for the status change to be completed.
newUpdateResolverConfig ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'autodefinedReverseFlag'
  AutodefinedReverseFlag ->
  UpdateResolverConfig
newUpdateResolverConfig
  pResourceId_
  pAutodefinedReverseFlag_ =
    UpdateResolverConfig'
      { resourceId = pResourceId_,
        autodefinedReverseFlag = pAutodefinedReverseFlag_
      }

-- | Resource ID of the Amazon VPC that you want to update the Resolver
-- configuration for.
updateResolverConfig_resourceId :: Lens.Lens' UpdateResolverConfig Prelude.Text
updateResolverConfig_resourceId = Lens.lens (\UpdateResolverConfig' {resourceId} -> resourceId) (\s@UpdateResolverConfig' {} a -> s {resourceId = a} :: UpdateResolverConfig)

-- | Indicates whether or not the Resolver will create autodefined rules for
-- reverse DNS lookups. This is enabled by default. Disabling this option
-- will also affect EC2-Classic instances using ClassicLink. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 guide/.
--
-- It can take some time for the status change to be completed.
updateResolverConfig_autodefinedReverseFlag :: Lens.Lens' UpdateResolverConfig AutodefinedReverseFlag
updateResolverConfig_autodefinedReverseFlag = Lens.lens (\UpdateResolverConfig' {autodefinedReverseFlag} -> autodefinedReverseFlag) (\s@UpdateResolverConfig' {} a -> s {autodefinedReverseFlag = a} :: UpdateResolverConfig)

instance Core.AWSRequest UpdateResolverConfig where
  type
    AWSResponse UpdateResolverConfig =
      UpdateResolverConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResolverConfigResponse'
            Prelude.<$> (x Data..?> "ResolverConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResolverConfig where
  hashWithSalt _salt UpdateResolverConfig' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` autodefinedReverseFlag

instance Prelude.NFData UpdateResolverConfig where
  rnf UpdateResolverConfig' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf autodefinedReverseFlag

instance Data.ToHeaders UpdateResolverConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.UpdateResolverConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResolverConfig where
  toJSON UpdateResolverConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ( "AutodefinedReverseFlag"
                  Data..= autodefinedReverseFlag
              )
          ]
      )

instance Data.ToPath UpdateResolverConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateResolverConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResolverConfigResponse' smart constructor.
data UpdateResolverConfigResponse = UpdateResolverConfigResponse'
  { -- | An array that contains settings for the specified Resolver
    -- configuration.
    resolverConfig :: Prelude.Maybe ResolverConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResolverConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverConfig', 'updateResolverConfigResponse_resolverConfig' - An array that contains settings for the specified Resolver
-- configuration.
--
-- 'httpStatus', 'updateResolverConfigResponse_httpStatus' - The response's http status code.
newUpdateResolverConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResolverConfigResponse
newUpdateResolverConfigResponse pHttpStatus_ =
  UpdateResolverConfigResponse'
    { resolverConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains settings for the specified Resolver
-- configuration.
updateResolverConfigResponse_resolverConfig :: Lens.Lens' UpdateResolverConfigResponse (Prelude.Maybe ResolverConfig)
updateResolverConfigResponse_resolverConfig = Lens.lens (\UpdateResolverConfigResponse' {resolverConfig} -> resolverConfig) (\s@UpdateResolverConfigResponse' {} a -> s {resolverConfig = a} :: UpdateResolverConfigResponse)

-- | The response's http status code.
updateResolverConfigResponse_httpStatus :: Lens.Lens' UpdateResolverConfigResponse Prelude.Int
updateResolverConfigResponse_httpStatus = Lens.lens (\UpdateResolverConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateResolverConfigResponse' {} a -> s {httpStatus = a} :: UpdateResolverConfigResponse)

instance Prelude.NFData UpdateResolverConfigResponse where
  rnf UpdateResolverConfigResponse' {..} =
    Prelude.rnf resolverConfig
      `Prelude.seq` Prelude.rnf httpStatus
