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
-- Module      : Amazonka.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.OpsWorks.DescribeLayers
  ( -- * Creating a Request
    DescribeLayers (..),
    newDescribeLayers,

    -- * Request Lenses
    describeLayers_layerIds,
    describeLayers_stackId,

    -- * Destructuring the Response
    DescribeLayersResponse (..),
    newDescribeLayersResponse,

    -- * Response Lenses
    describeLayersResponse_layers,
    describeLayersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { -- | An array of layer IDs that specify the layers to be described. If you
    -- omit this parameter, @DescribeLayers@ returns a description of every
    -- layer in the specified stack.
    layerIds :: Prelude.Maybe [Prelude.Text],
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLayers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerIds', 'describeLayers_layerIds' - An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
--
-- 'stackId', 'describeLayers_stackId' - The stack ID.
newDescribeLayers ::
  DescribeLayers
newDescribeLayers =
  DescribeLayers'
    { layerIds = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
describeLayers_layerIds :: Lens.Lens' DescribeLayers (Prelude.Maybe [Prelude.Text])
describeLayers_layerIds = Lens.lens (\DescribeLayers' {layerIds} -> layerIds) (\s@DescribeLayers' {} a -> s {layerIds = a} :: DescribeLayers) Prelude.. Lens.mapping Lens.coerced

-- | The stack ID.
describeLayers_stackId :: Lens.Lens' DescribeLayers (Prelude.Maybe Prelude.Text)
describeLayers_stackId = Lens.lens (\DescribeLayers' {stackId} -> stackId) (\s@DescribeLayers' {} a -> s {stackId = a} :: DescribeLayers)

instance Core.AWSRequest DescribeLayers where
  type
    AWSResponse DescribeLayers =
      DescribeLayersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLayersResponse'
            Prelude.<$> (x Data..?> "Layers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLayers where
  hashWithSalt _salt DescribeLayers' {..} =
    _salt `Prelude.hashWithSalt` layerIds
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeLayers where
  rnf DescribeLayers' {..} =
    Prelude.rnf layerIds
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeLayers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeLayers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLayers where
  toJSON DescribeLayers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LayerIds" Data..=) Prelude.<$> layerIds,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeLayers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLayers where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeLayersResponse_layers = Lens.lens (\DescribeLayersResponse' {layers} -> layers) (\s@DescribeLayersResponse' {} a -> s {layers = a} :: DescribeLayersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLayersResponse_httpStatus :: Lens.Lens' DescribeLayersResponse Prelude.Int
describeLayersResponse_httpStatus = Lens.lens (\DescribeLayersResponse' {httpStatus} -> httpStatus) (\s@DescribeLayersResponse' {} a -> s {httpStatus = a} :: DescribeLayersResponse)

instance Prelude.NFData DescribeLayersResponse where
  rnf DescribeLayersResponse' {..} =
    Prelude.rnf layers
      `Prelude.seq` Prelude.rnf httpStatus
