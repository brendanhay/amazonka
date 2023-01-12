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
-- Module      : Amazonka.IoTWireless.GetResourceLogLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the log-level override, if any, for a given resource-ID and
-- resource-type. It can be used for a wireless device or a wireless
-- gateway.
module Amazonka.IoTWireless.GetResourceLogLevel
  ( -- * Creating a Request
    GetResourceLogLevel (..),
    newGetResourceLogLevel,

    -- * Request Lenses
    getResourceLogLevel_resourceIdentifier,
    getResourceLogLevel_resourceType,

    -- * Destructuring the Response
    GetResourceLogLevelResponse (..),
    newGetResourceLogLevelResponse,

    -- * Response Lenses
    getResourceLogLevelResponse_logLevel,
    getResourceLogLevelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceLogLevel' smart constructor.
data GetResourceLogLevel = GetResourceLogLevel'
  { resourceIdentifier :: Prelude.Text,
    -- | The type of the resource, which can be @WirelessDevice@ or
    -- @WirelessGateway@.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceLogLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getResourceLogLevel_resourceIdentifier' - Undocumented member.
--
-- 'resourceType', 'getResourceLogLevel_resourceType' - The type of the resource, which can be @WirelessDevice@ or
-- @WirelessGateway@.
newGetResourceLogLevel ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  GetResourceLogLevel
newGetResourceLogLevel
  pResourceIdentifier_
  pResourceType_ =
    GetResourceLogLevel'
      { resourceIdentifier =
          pResourceIdentifier_,
        resourceType = pResourceType_
      }

-- | Undocumented member.
getResourceLogLevel_resourceIdentifier :: Lens.Lens' GetResourceLogLevel Prelude.Text
getResourceLogLevel_resourceIdentifier = Lens.lens (\GetResourceLogLevel' {resourceIdentifier} -> resourceIdentifier) (\s@GetResourceLogLevel' {} a -> s {resourceIdentifier = a} :: GetResourceLogLevel)

-- | The type of the resource, which can be @WirelessDevice@ or
-- @WirelessGateway@.
getResourceLogLevel_resourceType :: Lens.Lens' GetResourceLogLevel Prelude.Text
getResourceLogLevel_resourceType = Lens.lens (\GetResourceLogLevel' {resourceType} -> resourceType) (\s@GetResourceLogLevel' {} a -> s {resourceType = a} :: GetResourceLogLevel)

instance Core.AWSRequest GetResourceLogLevel where
  type
    AWSResponse GetResourceLogLevel =
      GetResourceLogLevelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceLogLevelResponse'
            Prelude.<$> (x Data..?> "LogLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceLogLevel where
  hashWithSalt _salt GetResourceLogLevel' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetResourceLogLevel where
  rnf GetResourceLogLevel' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GetResourceLogLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetResourceLogLevel where
  toPath GetResourceLogLevel' {..} =
    Prelude.mconcat
      ["/log-levels/", Data.toBS resourceIdentifier]

instance Data.ToQuery GetResourceLogLevel where
  toQuery GetResourceLogLevel' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newGetResourceLogLevelResponse' smart constructor.
data GetResourceLogLevelResponse = GetResourceLogLevelResponse'
  { logLevel :: Prelude.Maybe LogLevel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceLogLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'getResourceLogLevelResponse_logLevel' - Undocumented member.
--
-- 'httpStatus', 'getResourceLogLevelResponse_httpStatus' - The response's http status code.
newGetResourceLogLevelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceLogLevelResponse
newGetResourceLogLevelResponse pHttpStatus_ =
  GetResourceLogLevelResponse'
    { logLevel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getResourceLogLevelResponse_logLevel :: Lens.Lens' GetResourceLogLevelResponse (Prelude.Maybe LogLevel)
getResourceLogLevelResponse_logLevel = Lens.lens (\GetResourceLogLevelResponse' {logLevel} -> logLevel) (\s@GetResourceLogLevelResponse' {} a -> s {logLevel = a} :: GetResourceLogLevelResponse)

-- | The response's http status code.
getResourceLogLevelResponse_httpStatus :: Lens.Lens' GetResourceLogLevelResponse Prelude.Int
getResourceLogLevelResponse_httpStatus = Lens.lens (\GetResourceLogLevelResponse' {httpStatus} -> httpStatus) (\s@GetResourceLogLevelResponse' {} a -> s {httpStatus = a} :: GetResourceLogLevelResponse)

instance Prelude.NFData GetResourceLogLevelResponse where
  rnf GetResourceLogLevelResponse' {..} =
    Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf httpStatus
