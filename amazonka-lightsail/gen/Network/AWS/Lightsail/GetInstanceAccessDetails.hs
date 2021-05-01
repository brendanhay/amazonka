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
-- Module      : Network.AWS.Lightsail.GetInstanceAccessDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.GetInstanceAccessDetails
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstanceAccessDetails' smart constructor.
data GetInstanceAccessDetails = GetInstanceAccessDetails'
  { -- | The protocol to use to connect to your instance. Defaults to @ssh@.
    protocol :: Prelude.Maybe InstanceAccessProtocol,
    -- | The name of the instance to access.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetInstanceAccessDetails where
  type
    Rs GetInstanceAccessDetails =
      GetInstanceAccessDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceAccessDetailsResponse'
            Prelude.<$> (x Prelude..?> "accessDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceAccessDetails

instance Prelude.NFData GetInstanceAccessDetails

instance Prelude.ToHeaders GetInstanceAccessDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetInstanceAccessDetails" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetInstanceAccessDetails where
  toJSON GetInstanceAccessDetails' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("protocol" Prelude..=) Prelude.<$> protocol,
            Prelude.Just
              ("instanceName" Prelude..= instanceName)
          ]
      )

instance Prelude.ToPath GetInstanceAccessDetails where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetInstanceAccessDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceAccessDetailsResponse' smart constructor.
data GetInstanceAccessDetailsResponse = GetInstanceAccessDetailsResponse'
  { -- | An array of key-value pairs containing information about a get instance
    -- access request.
    accessDetails :: Prelude.Maybe InstanceAccessDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
