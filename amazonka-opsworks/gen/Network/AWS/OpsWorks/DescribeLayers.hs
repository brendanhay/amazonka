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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | An array of layer IDs that specify the layers to be described. If you
    -- omit this parameter, @DescribeLayers@ returns a description of every
    -- layer in the specified stack.
    layerIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stackId = Prelude.Nothing,
      layerIds = Prelude.Nothing
    }

-- | The stack ID.
describeLayers_stackId :: Lens.Lens' DescribeLayers (Prelude.Maybe Prelude.Text)
describeLayers_stackId = Lens.lens (\DescribeLayers' {stackId} -> stackId) (\s@DescribeLayers' {} a -> s {stackId = a} :: DescribeLayers)

-- | An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
describeLayers_layerIds :: Lens.Lens' DescribeLayers (Prelude.Maybe [Prelude.Text])
describeLayers_layerIds = Lens.lens (\DescribeLayers' {layerIds} -> layerIds) (\s@DescribeLayers' {} a -> s {layerIds = a} :: DescribeLayers) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribeLayers where
  type Rs DescribeLayers = DescribeLayersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLayersResponse'
            Prelude.<$> (x Prelude..?> "Layers" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLayers

instance Prelude.NFData DescribeLayers

instance Prelude.ToHeaders DescribeLayers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DescribeLayers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeLayers where
  toJSON DescribeLayers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StackId" Prelude..=) Prelude.<$> stackId,
            ("LayerIds" Prelude..=) Prelude.<$> layerIds
          ]
      )

instance Prelude.ToPath DescribeLayers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeLayers where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeLayers@ request.
--
-- /See:/ 'newDescribeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { -- | An array of @Layer@ objects that describe the layers.
    layers :: Prelude.Maybe [Layer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLayersResponse
newDescribeLayersResponse pHttpStatus_ =
  DescribeLayersResponse'
    { layers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Layer@ objects that describe the layers.
describeLayersResponse_layers :: Lens.Lens' DescribeLayersResponse (Prelude.Maybe [Layer])
describeLayersResponse_layers = Lens.lens (\DescribeLayersResponse' {layers} -> layers) (\s@DescribeLayersResponse' {} a -> s {layers = a} :: DescribeLayersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeLayersResponse_httpStatus :: Lens.Lens' DescribeLayersResponse Prelude.Int
describeLayersResponse_httpStatus = Lens.lens (\DescribeLayersResponse' {httpStatus} -> httpStatus) (\s@DescribeLayersResponse' {} a -> s {httpStatus = a} :: DescribeLayersResponse)

instance Prelude.NFData DescribeLayersResponse
