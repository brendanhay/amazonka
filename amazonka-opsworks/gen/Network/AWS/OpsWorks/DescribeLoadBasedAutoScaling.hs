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
-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLoadBasedAutoScaling' smart constructor.
data DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
  { -- | An array of layer IDs.
    layerIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | An array of layer IDs.
describeLoadBasedAutoScaling_layerIds :: Lens.Lens' DescribeLoadBasedAutoScaling [Core.Text]
describeLoadBasedAutoScaling_layerIds = Lens.lens (\DescribeLoadBasedAutoScaling' {layerIds} -> layerIds) (\s@DescribeLoadBasedAutoScaling' {} a -> s {layerIds = a} :: DescribeLoadBasedAutoScaling) Core.. Lens._Coerce

instance Core.AWSRequest DescribeLoadBasedAutoScaling where
  type
    AWSResponse DescribeLoadBasedAutoScaling =
      DescribeLoadBasedAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoadBasedAutoScalingResponse'
            Core.<$> ( x Core..?> "LoadBasedAutoScalingConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLoadBasedAutoScaling

instance Core.NFData DescribeLoadBasedAutoScaling

instance Core.ToHeaders DescribeLoadBasedAutoScaling where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeLoadBasedAutoScaling" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLoadBasedAutoScaling where
  toJSON DescribeLoadBasedAutoScaling' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("LayerIds" Core..= layerIds)]
      )

instance Core.ToPath DescribeLoadBasedAutoScaling where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLoadBasedAutoScaling where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'newDescribeLoadBasedAutoScalingResponse' smart constructor.
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
  { -- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
    -- each layer\'s configuration.
    loadBasedAutoScalingConfigurations :: Core.Maybe [LoadBasedAutoScalingConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeLoadBasedAutoScalingResponse
newDescribeLoadBasedAutoScalingResponse pHttpStatus_ =
  DescribeLoadBasedAutoScalingResponse'
    { loadBasedAutoScalingConfigurations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
-- each layer\'s configuration.
describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations :: Lens.Lens' DescribeLoadBasedAutoScalingResponse (Core.Maybe [LoadBasedAutoScalingConfiguration])
describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations = Lens.lens (\DescribeLoadBasedAutoScalingResponse' {loadBasedAutoScalingConfigurations} -> loadBasedAutoScalingConfigurations) (\s@DescribeLoadBasedAutoScalingResponse' {} a -> s {loadBasedAutoScalingConfigurations = a} :: DescribeLoadBasedAutoScalingResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLoadBasedAutoScalingResponse_httpStatus :: Lens.Lens' DescribeLoadBasedAutoScalingResponse Core.Int
describeLoadBasedAutoScalingResponse_httpStatus = Lens.lens (\DescribeLoadBasedAutoScalingResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBasedAutoScalingResponse' {} a -> s {httpStatus = a} :: DescribeLoadBasedAutoScalingResponse)

instance
  Core.NFData
    DescribeLoadBasedAutoScalingResponse
