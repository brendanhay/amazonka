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
-- Module      : Amazonka.PrivateNetworks.StartNetworkResourceUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an update of the specified network resource.
--
-- After you submit a request to replace or return a network resource, the
-- status of the network resource is @CREATING_SHIPPING_LABEL@. The
-- shipping label is available when the status of the network resource is
-- @PENDING_RETURN@. After the network resource is successfully returned,
-- its status is @DELETED@. For more information, see
-- <https://docs.aws.amazon.com/private-networks/latest/userguide/radio-units.html#return-radio-unit Return a radio unit>.
module Amazonka.PrivateNetworks.StartNetworkResourceUpdate
  ( -- * Creating a Request
    StartNetworkResourceUpdate (..),
    newStartNetworkResourceUpdate,

    -- * Request Lenses
    startNetworkResourceUpdate_returnReason,
    startNetworkResourceUpdate_shippingAddress,
    startNetworkResourceUpdate_networkResourceArn,
    startNetworkResourceUpdate_updateType,

    -- * Destructuring the Response
    StartNetworkResourceUpdateResponse (..),
    newStartNetworkResourceUpdateResponse,

    -- * Response Lenses
    startNetworkResourceUpdateResponse_networkResource,
    startNetworkResourceUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartNetworkResourceUpdate' smart constructor.
data StartNetworkResourceUpdate = StartNetworkResourceUpdate'
  { -- | The reason for the return. Providing a reason for a return is optional.
    returnReason :: Prelude.Maybe Prelude.Text,
    -- | The shipping address. If you don\'t provide a shipping address when
    -- replacing or returning a network resource, we use the address from the
    -- original order for the network resource.
    shippingAddress :: Prelude.Maybe Address,
    -- | The Amazon Resource Name (ARN) of the network resource.
    networkResourceArn :: Prelude.Text,
    -- | The update type.
    --
    -- -   @REPLACE@ - Submits a request to replace a defective radio unit. We
    --     provide a shipping label that you can use for the return process and
    --     we ship a replacement radio unit to you.
    --
    -- -   @RETURN@ - Submits a request to replace a radio unit that you no
    --     longer need. We provide a shipping label that you can use for the
    --     return process.
    updateType :: UpdateType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkResourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnReason', 'startNetworkResourceUpdate_returnReason' - The reason for the return. Providing a reason for a return is optional.
--
-- 'shippingAddress', 'startNetworkResourceUpdate_shippingAddress' - The shipping address. If you don\'t provide a shipping address when
-- replacing or returning a network resource, we use the address from the
-- original order for the network resource.
--
-- 'networkResourceArn', 'startNetworkResourceUpdate_networkResourceArn' - The Amazon Resource Name (ARN) of the network resource.
--
-- 'updateType', 'startNetworkResourceUpdate_updateType' - The update type.
--
-- -   @REPLACE@ - Submits a request to replace a defective radio unit. We
--     provide a shipping label that you can use for the return process and
--     we ship a replacement radio unit to you.
--
-- -   @RETURN@ - Submits a request to replace a radio unit that you no
--     longer need. We provide a shipping label that you can use for the
--     return process.
newStartNetworkResourceUpdate ::
  -- | 'networkResourceArn'
  Prelude.Text ->
  -- | 'updateType'
  UpdateType ->
  StartNetworkResourceUpdate
newStartNetworkResourceUpdate
  pNetworkResourceArn_
  pUpdateType_ =
    StartNetworkResourceUpdate'
      { returnReason =
          Prelude.Nothing,
        shippingAddress = Prelude.Nothing,
        networkResourceArn = pNetworkResourceArn_,
        updateType = pUpdateType_
      }

-- | The reason for the return. Providing a reason for a return is optional.
startNetworkResourceUpdate_returnReason :: Lens.Lens' StartNetworkResourceUpdate (Prelude.Maybe Prelude.Text)
startNetworkResourceUpdate_returnReason = Lens.lens (\StartNetworkResourceUpdate' {returnReason} -> returnReason) (\s@StartNetworkResourceUpdate' {} a -> s {returnReason = a} :: StartNetworkResourceUpdate)

-- | The shipping address. If you don\'t provide a shipping address when
-- replacing or returning a network resource, we use the address from the
-- original order for the network resource.
startNetworkResourceUpdate_shippingAddress :: Lens.Lens' StartNetworkResourceUpdate (Prelude.Maybe Address)
startNetworkResourceUpdate_shippingAddress = Lens.lens (\StartNetworkResourceUpdate' {shippingAddress} -> shippingAddress) (\s@StartNetworkResourceUpdate' {} a -> s {shippingAddress = a} :: StartNetworkResourceUpdate)

-- | The Amazon Resource Name (ARN) of the network resource.
startNetworkResourceUpdate_networkResourceArn :: Lens.Lens' StartNetworkResourceUpdate Prelude.Text
startNetworkResourceUpdate_networkResourceArn = Lens.lens (\StartNetworkResourceUpdate' {networkResourceArn} -> networkResourceArn) (\s@StartNetworkResourceUpdate' {} a -> s {networkResourceArn = a} :: StartNetworkResourceUpdate)

-- | The update type.
--
-- -   @REPLACE@ - Submits a request to replace a defective radio unit. We
--     provide a shipping label that you can use for the return process and
--     we ship a replacement radio unit to you.
--
-- -   @RETURN@ - Submits a request to replace a radio unit that you no
--     longer need. We provide a shipping label that you can use for the
--     return process.
startNetworkResourceUpdate_updateType :: Lens.Lens' StartNetworkResourceUpdate UpdateType
startNetworkResourceUpdate_updateType = Lens.lens (\StartNetworkResourceUpdate' {updateType} -> updateType) (\s@StartNetworkResourceUpdate' {} a -> s {updateType = a} :: StartNetworkResourceUpdate)

instance Core.AWSRequest StartNetworkResourceUpdate where
  type
    AWSResponse StartNetworkResourceUpdate =
      StartNetworkResourceUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartNetworkResourceUpdateResponse'
            Prelude.<$> (x Data..?> "networkResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartNetworkResourceUpdate where
  hashWithSalt _salt StartNetworkResourceUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` returnReason
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` networkResourceArn
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData StartNetworkResourceUpdate where
  rnf StartNetworkResourceUpdate' {..} =
    Prelude.rnf returnReason
      `Prelude.seq` Prelude.rnf shippingAddress
      `Prelude.seq` Prelude.rnf networkResourceArn
      `Prelude.seq` Prelude.rnf updateType

instance Data.ToHeaders StartNetworkResourceUpdate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartNetworkResourceUpdate where
  toJSON StartNetworkResourceUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("returnReason" Data..=) Prelude.<$> returnReason,
            ("shippingAddress" Data..=)
              Prelude.<$> shippingAddress,
            Prelude.Just
              ("networkResourceArn" Data..= networkResourceArn),
            Prelude.Just ("updateType" Data..= updateType)
          ]
      )

instance Data.ToPath StartNetworkResourceUpdate where
  toPath = Prelude.const "/v1/network-resources/update"

instance Data.ToQuery StartNetworkResourceUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartNetworkResourceUpdateResponse' smart constructor.
data StartNetworkResourceUpdateResponse = StartNetworkResourceUpdateResponse'
  { -- | The network resource.
    networkResource :: Prelude.Maybe NetworkResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkResourceUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkResource', 'startNetworkResourceUpdateResponse_networkResource' - The network resource.
--
-- 'httpStatus', 'startNetworkResourceUpdateResponse_httpStatus' - The response's http status code.
newStartNetworkResourceUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartNetworkResourceUpdateResponse
newStartNetworkResourceUpdateResponse pHttpStatus_ =
  StartNetworkResourceUpdateResponse'
    { networkResource =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network resource.
startNetworkResourceUpdateResponse_networkResource :: Lens.Lens' StartNetworkResourceUpdateResponse (Prelude.Maybe NetworkResource)
startNetworkResourceUpdateResponse_networkResource = Lens.lens (\StartNetworkResourceUpdateResponse' {networkResource} -> networkResource) (\s@StartNetworkResourceUpdateResponse' {} a -> s {networkResource = a} :: StartNetworkResourceUpdateResponse)

-- | The response's http status code.
startNetworkResourceUpdateResponse_httpStatus :: Lens.Lens' StartNetworkResourceUpdateResponse Prelude.Int
startNetworkResourceUpdateResponse_httpStatus = Lens.lens (\StartNetworkResourceUpdateResponse' {httpStatus} -> httpStatus) (\s@StartNetworkResourceUpdateResponse' {} a -> s {httpStatus = a} :: StartNetworkResourceUpdateResponse)

instance
  Prelude.NFData
    StartNetworkResourceUpdateResponse
  where
  rnf StartNetworkResourceUpdateResponse' {..} =
    Prelude.rnf networkResource
      `Prelude.seq` Prelude.rnf httpStatus
