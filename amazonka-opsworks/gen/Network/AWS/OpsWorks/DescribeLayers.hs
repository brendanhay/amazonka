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
-- Module      : Network.AWS.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more layers in a specified stack.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeLayers
  ( -- * Creating a Request
    DescribeLayers (..),
    newDescribeLayers,

    -- * Request Lenses
    describeLayers_stackId,
    describeLayers_layerIds,

    -- * Destructuring the Response
    DescribeLayersResponse (..),
    newDescribeLayersResponse,

    -- * Response Lenses
    describeLayersResponse_layers,
    describeLayersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | An array of layer IDs that specify the layers to be described. If you
    -- omit this parameter, @DescribeLayers@ returns a description of every
    -- layer in the specified stack.
    layerIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLayers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeLayers_stackId' - The stack ID.
--
-- 'layerIds', 'describeLayers_layerIds' - An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
newDescribeLayers ::
  DescribeLayers
newDescribeLayers =
  DescribeLayers'
    { stackId = Core.Nothing,
      layerIds = Core.Nothing
    }

-- | The stack ID.
describeLayers_stackId :: Lens.Lens' DescribeLayers (Core.Maybe Core.Text)
describeLayers_stackId = Lens.lens (\DescribeLayers' {stackId} -> stackId) (\s@DescribeLayers' {} a -> s {stackId = a} :: DescribeLayers)

-- | An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
describeLayers_layerIds :: Lens.Lens' DescribeLayers (Core.Maybe [Core.Text])
describeLayers_layerIds = Lens.lens (\DescribeLayers' {layerIds} -> layerIds) (\s@DescribeLayers' {} a -> s {layerIds = a} :: DescribeLayers) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeLayers where
  type
    AWSResponse DescribeLayers =
      DescribeLayersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLayersResponse'
            Core.<$> (x Core..?> "Layers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLayers

instance Core.NFData DescribeLayers

instance Core.ToHeaders DescribeLayers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeLayers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLayers where
  toJSON DescribeLayers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StackId" Core..=) Core.<$> stackId,
            ("LayerIds" Core..=) Core.<$> layerIds
          ]
      )

instance Core.ToPath DescribeLayers where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLayers where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeLayers@ request.
--
-- /See:/ 'newDescribeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { -- | An array of @Layer@ objects that describe the layers.
    layers :: Core.Maybe [Layer],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLayersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layers', 'describeLayersResponse_layers' - An array of @Layer@ objects that describe the layers.
--
-- 'httpStatus', 'describeLayersResponse_httpStatus' - The response's http status code.
newDescribeLayersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLayersResponse
newDescribeLayersResponse pHttpStatus_ =
  DescribeLayersResponse'
    { layers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Layer@ objects that describe the layers.
describeLayersResponse_layers :: Lens.Lens' DescribeLayersResponse (Core.Maybe [Layer])
describeLayersResponse_layers = Lens.lens (\DescribeLayersResponse' {layers} -> layers) (\s@DescribeLayersResponse' {} a -> s {layers = a} :: DescribeLayersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLayersResponse_httpStatus :: Lens.Lens' DescribeLayersResponse Core.Int
describeLayersResponse_httpStatus = Lens.lens (\DescribeLayersResponse' {httpStatus} -> httpStatus) (\s@DescribeLayersResponse' {} a -> s {httpStatus = a} :: DescribeLayersResponse)

instance Core.NFData DescribeLayersResponse
