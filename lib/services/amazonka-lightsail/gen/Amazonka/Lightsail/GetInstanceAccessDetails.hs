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
-- Module      : Amazonka.Lightsail.GetInstanceAccessDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns temporary SSH keys you can use to connect to a specific virtual
-- private server, or /instance/.
--
-- The @get instance access details@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- @instance name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.GetInstanceAccessDetails
  ( -- * Creating a Request
    GetInstanceAccessDetails (..),
    newGetInstanceAccessDetails,

    -- * Request Lenses
    getInstanceAccessDetails_protocol,
    getInstanceAccessDetails_instanceName,

    -- * Destructuring the Response
    GetInstanceAccessDetailsResponse (..),
    newGetInstanceAccessDetailsResponse,

    -- * Response Lenses
    getInstanceAccessDetailsResponse_accessDetails,
    getInstanceAccessDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceAccessDetails' smart constructor.
data GetInstanceAccessDetails = GetInstanceAccessDetails'
  { -- | The protocol to use to connect to your instance. Defaults to @ssh@.
    protocol :: Prelude.Maybe InstanceAccessProtocol,
    -- | The name of the instance to access.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceAccessDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'getInstanceAccessDetails_protocol' - The protocol to use to connect to your instance. Defaults to @ssh@.
--
-- 'instanceName', 'getInstanceAccessDetails_instanceName' - The name of the instance to access.
newGetInstanceAccessDetails ::
  -- | 'instanceName'
  Prelude.Text ->
  GetInstanceAccessDetails
newGetInstanceAccessDetails pInstanceName_ =
  GetInstanceAccessDetails'
    { protocol =
        Prelude.Nothing,
      instanceName = pInstanceName_
    }

-- | The protocol to use to connect to your instance. Defaults to @ssh@.
getInstanceAccessDetails_protocol :: Lens.Lens' GetInstanceAccessDetails (Prelude.Maybe InstanceAccessProtocol)
getInstanceAccessDetails_protocol = Lens.lens (\GetInstanceAccessDetails' {protocol} -> protocol) (\s@GetInstanceAccessDetails' {} a -> s {protocol = a} :: GetInstanceAccessDetails)

-- | The name of the instance to access.
getInstanceAccessDetails_instanceName :: Lens.Lens' GetInstanceAccessDetails Prelude.Text
getInstanceAccessDetails_instanceName = Lens.lens (\GetInstanceAccessDetails' {instanceName} -> instanceName) (\s@GetInstanceAccessDetails' {} a -> s {instanceName = a} :: GetInstanceAccessDetails)

instance Core.AWSRequest GetInstanceAccessDetails where
  type
    AWSResponse GetInstanceAccessDetails =
      GetInstanceAccessDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceAccessDetailsResponse'
            Prelude.<$> (x Data..?> "accessDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceAccessDetails where
  hashWithSalt _salt GetInstanceAccessDetails' {..} =
    _salt
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData GetInstanceAccessDetails where
  rnf GetInstanceAccessDetails' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders GetInstanceAccessDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetInstanceAccessDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstanceAccessDetails where
  toJSON GetInstanceAccessDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("protocol" Data..=) Prelude.<$> protocol,
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath GetInstanceAccessDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstanceAccessDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceAccessDetailsResponse' smart constructor.
data GetInstanceAccessDetailsResponse = GetInstanceAccessDetailsResponse'
  { -- | An array of key-value pairs containing information about a get instance
    -- access request.
    accessDetails :: Prelude.Maybe InstanceAccessDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceAccessDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessDetails', 'getInstanceAccessDetailsResponse_accessDetails' - An array of key-value pairs containing information about a get instance
-- access request.
--
-- 'httpStatus', 'getInstanceAccessDetailsResponse_httpStatus' - The response's http status code.
newGetInstanceAccessDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceAccessDetailsResponse
newGetInstanceAccessDetailsResponse pHttpStatus_ =
  GetInstanceAccessDetailsResponse'
    { accessDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about a get instance
-- access request.
getInstanceAccessDetailsResponse_accessDetails :: Lens.Lens' GetInstanceAccessDetailsResponse (Prelude.Maybe InstanceAccessDetails)
getInstanceAccessDetailsResponse_accessDetails = Lens.lens (\GetInstanceAccessDetailsResponse' {accessDetails} -> accessDetails) (\s@GetInstanceAccessDetailsResponse' {} a -> s {accessDetails = a} :: GetInstanceAccessDetailsResponse)

-- | The response's http status code.
getInstanceAccessDetailsResponse_httpStatus :: Lens.Lens' GetInstanceAccessDetailsResponse Prelude.Int
getInstanceAccessDetailsResponse_httpStatus = Lens.lens (\GetInstanceAccessDetailsResponse' {httpStatus} -> httpStatus) (\s@GetInstanceAccessDetailsResponse' {} a -> s {httpStatus = a} :: GetInstanceAccessDetailsResponse)

instance
  Prelude.NFData
    GetInstanceAccessDetailsResponse
  where
  rnf GetInstanceAccessDetailsResponse' {..} =
    Prelude.rnf accessDetails
      `Prelude.seq` Prelude.rnf httpStatus
