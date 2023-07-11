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
-- Module      : Amazonka.GlobalAccelerator.AddEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add endpoints to an endpoint group. The @AddEndpoints@ API operation is
-- the recommended option for adding endpoints. The alternative options are
-- to add endpoints when you create an endpoint group (with the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_CreateEndpointGroup.html CreateEndpointGroup>
-- API) or when you update an endpoint group (with the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_UpdateEndpointGroup.html UpdateEndpointGroup>
-- API).
--
-- There are two advantages to using @AddEndpoints@ to add endpoints:
--
-- -   It\'s faster, because Global Accelerator only has to resolve the new
--     endpoints that you\'re adding.
--
-- -   It\'s more convenient, because you don\'t need to specify all of the
--     current endpoints that are already in the endpoint group in addition
--     to the new endpoints that you want to add.
module Amazonka.GlobalAccelerator.AddEndpoints
  ( -- * Creating a Request
    AddEndpoints (..),
    newAddEndpoints,

    -- * Request Lenses
    addEndpoints_endpointConfigurations,
    addEndpoints_endpointGroupArn,

    -- * Destructuring the Response
    AddEndpointsResponse (..),
    newAddEndpointsResponse,

    -- * Response Lenses
    addEndpointsResponse_endpointDescriptions,
    addEndpointsResponse_endpointGroupArn,
    addEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddEndpoints' smart constructor.
data AddEndpoints = AddEndpoints'
  { -- | The list of endpoint objects.
    endpointConfigurations :: [EndpointConfiguration],
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigurations', 'addEndpoints_endpointConfigurations' - The list of endpoint objects.
--
-- 'endpointGroupArn', 'addEndpoints_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
newAddEndpoints ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  AddEndpoints
newAddEndpoints pEndpointGroupArn_ =
  AddEndpoints'
    { endpointConfigurations =
        Prelude.mempty,
      endpointGroupArn = pEndpointGroupArn_
    }

-- | The list of endpoint objects.
addEndpoints_endpointConfigurations :: Lens.Lens' AddEndpoints [EndpointConfiguration]
addEndpoints_endpointConfigurations = Lens.lens (\AddEndpoints' {endpointConfigurations} -> endpointConfigurations) (\s@AddEndpoints' {} a -> s {endpointConfigurations = a} :: AddEndpoints) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
addEndpoints_endpointGroupArn :: Lens.Lens' AddEndpoints Prelude.Text
addEndpoints_endpointGroupArn = Lens.lens (\AddEndpoints' {endpointGroupArn} -> endpointGroupArn) (\s@AddEndpoints' {} a -> s {endpointGroupArn = a} :: AddEndpoints)

instance Core.AWSRequest AddEndpoints where
  type AWSResponse AddEndpoints = AddEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddEndpointsResponse'
            Prelude.<$> ( x
                            Data..?> "EndpointDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "EndpointGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddEndpoints where
  hashWithSalt _salt AddEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData AddEndpoints where
  rnf AddEndpoints' {..} =
    Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf endpointGroupArn

instance Data.ToHeaders AddEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.AddEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddEndpoints where
  toJSON AddEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EndpointConfigurations"
                  Data..= endpointConfigurations
              ),
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath AddEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery AddEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddEndpointsResponse' smart constructor.
data AddEndpointsResponse = AddEndpointsResponse'
  { -- | The list of endpoint objects.
    endpointDescriptions :: Prelude.Maybe [EndpointDescription],
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointDescriptions', 'addEndpointsResponse_endpointDescriptions' - The list of endpoint objects.
--
-- 'endpointGroupArn', 'addEndpointsResponse_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'httpStatus', 'addEndpointsResponse_httpStatus' - The response's http status code.
newAddEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddEndpointsResponse
newAddEndpointsResponse pHttpStatus_ =
  AddEndpointsResponse'
    { endpointDescriptions =
        Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of endpoint objects.
addEndpointsResponse_endpointDescriptions :: Lens.Lens' AddEndpointsResponse (Prelude.Maybe [EndpointDescription])
addEndpointsResponse_endpointDescriptions = Lens.lens (\AddEndpointsResponse' {endpointDescriptions} -> endpointDescriptions) (\s@AddEndpointsResponse' {} a -> s {endpointDescriptions = a} :: AddEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
addEndpointsResponse_endpointGroupArn :: Lens.Lens' AddEndpointsResponse (Prelude.Maybe Prelude.Text)
addEndpointsResponse_endpointGroupArn = Lens.lens (\AddEndpointsResponse' {endpointGroupArn} -> endpointGroupArn) (\s@AddEndpointsResponse' {} a -> s {endpointGroupArn = a} :: AddEndpointsResponse)

-- | The response's http status code.
addEndpointsResponse_httpStatus :: Lens.Lens' AddEndpointsResponse Prelude.Int
addEndpointsResponse_httpStatus = Lens.lens (\AddEndpointsResponse' {httpStatus} -> httpStatus) (\s@AddEndpointsResponse' {} a -> s {httpStatus = a} :: AddEndpointsResponse)

instance Prelude.NFData AddEndpointsResponse where
  rnf AddEndpointsResponse' {..} =
    Prelude.rnf endpointDescriptions
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
