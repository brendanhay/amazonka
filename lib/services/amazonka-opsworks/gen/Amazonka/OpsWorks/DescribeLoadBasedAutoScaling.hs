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
-- Module      : Amazonka.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes load-based auto scaling configurations for specified layers.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribeLoadBasedAutoScaling
  ( -- * Creating a Request
    DescribeLoadBasedAutoScaling (..),
    newDescribeLoadBasedAutoScaling,

    -- * Request Lenses
    describeLoadBasedAutoScaling_layerIds,

    -- * Destructuring the Response
    DescribeLoadBasedAutoScalingResponse (..),
    newDescribeLoadBasedAutoScalingResponse,

    -- * Response Lenses
    describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations,
    describeLoadBasedAutoScalingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLoadBasedAutoScaling' smart constructor.
data DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
  { -- | An array of layer IDs.
    layerIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBasedAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerIds', 'describeLoadBasedAutoScaling_layerIds' - An array of layer IDs.
newDescribeLoadBasedAutoScaling ::
  DescribeLoadBasedAutoScaling
newDescribeLoadBasedAutoScaling =
  DescribeLoadBasedAutoScaling'
    { layerIds =
        Prelude.mempty
    }

-- | An array of layer IDs.
describeLoadBasedAutoScaling_layerIds :: Lens.Lens' DescribeLoadBasedAutoScaling [Prelude.Text]
describeLoadBasedAutoScaling_layerIds = Lens.lens (\DescribeLoadBasedAutoScaling' {layerIds} -> layerIds) (\s@DescribeLoadBasedAutoScaling' {} a -> s {layerIds = a} :: DescribeLoadBasedAutoScaling) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeLoadBasedAutoScaling where
  type
    AWSResponse DescribeLoadBasedAutoScaling =
      DescribeLoadBasedAutoScalingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoadBasedAutoScalingResponse'
            Prelude.<$> ( x
                            Data..?> "LoadBasedAutoScalingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBasedAutoScaling
  where
  hashWithSalt _salt DescribeLoadBasedAutoScaling' {..} =
    _salt `Prelude.hashWithSalt` layerIds

instance Prelude.NFData DescribeLoadBasedAutoScaling where
  rnf DescribeLoadBasedAutoScaling' {..} =
    Prelude.rnf layerIds

instance Data.ToHeaders DescribeLoadBasedAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeLoadBasedAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLoadBasedAutoScaling where
  toJSON DescribeLoadBasedAutoScaling' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LayerIds" Data..= layerIds)]
      )

instance Data.ToPath DescribeLoadBasedAutoScaling where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLoadBasedAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'newDescribeLoadBasedAutoScalingResponse' smart constructor.
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
  { -- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
    -- each layer\'s configuration.
    loadBasedAutoScalingConfigurations :: Prelude.Maybe [LoadBasedAutoScalingConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBasedAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBasedAutoScalingConfigurations', 'describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations' - An array of @LoadBasedAutoScalingConfiguration@ objects that describe
-- each layer\'s configuration.
--
-- 'httpStatus', 'describeLoadBasedAutoScalingResponse_httpStatus' - The response's http status code.
newDescribeLoadBasedAutoScalingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBasedAutoScalingResponse
newDescribeLoadBasedAutoScalingResponse pHttpStatus_ =
  DescribeLoadBasedAutoScalingResponse'
    { loadBasedAutoScalingConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
-- each layer\'s configuration.
describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations :: Lens.Lens' DescribeLoadBasedAutoScalingResponse (Prelude.Maybe [LoadBasedAutoScalingConfiguration])
describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations = Lens.lens (\DescribeLoadBasedAutoScalingResponse' {loadBasedAutoScalingConfigurations} -> loadBasedAutoScalingConfigurations) (\s@DescribeLoadBasedAutoScalingResponse' {} a -> s {loadBasedAutoScalingConfigurations = a} :: DescribeLoadBasedAutoScalingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLoadBasedAutoScalingResponse_httpStatus :: Lens.Lens' DescribeLoadBasedAutoScalingResponse Prelude.Int
describeLoadBasedAutoScalingResponse_httpStatus = Lens.lens (\DescribeLoadBasedAutoScalingResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBasedAutoScalingResponse' {} a -> s {httpStatus = a} :: DescribeLoadBasedAutoScalingResponse)

instance
  Prelude.NFData
    DescribeLoadBasedAutoScalingResponse
  where
  rnf DescribeLoadBasedAutoScalingResponse' {..} =
    Prelude.rnf loadBasedAutoScalingConfigurations `Prelude.seq`
      Prelude.rnf httpStatus
