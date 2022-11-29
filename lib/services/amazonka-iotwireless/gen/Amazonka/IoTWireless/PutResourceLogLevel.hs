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
-- Module      : Amazonka.IoTWireless.PutResourceLogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the log-level override for a resource-ID and resource-type. This
-- option can be specified for a wireless gateway or a wireless device. A
-- limit of 200 log level override can be set per account.
module Amazonka.IoTWireless.PutResourceLogLevel
  ( -- * Creating a Request
    PutResourceLogLevel (..),
    newPutResourceLogLevel,

    -- * Request Lenses
    putResourceLogLevel_resourceIdentifier,
    putResourceLogLevel_resourceType,
    putResourceLogLevel_logLevel,

    -- * Destructuring the Response
    PutResourceLogLevelResponse (..),
    newPutResourceLogLevelResponse,

    -- * Response Lenses
    putResourceLogLevelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourceLogLevel' smart constructor.
data PutResourceLogLevel = PutResourceLogLevel'
  { resourceIdentifier :: Prelude.Text,
    -- | The type of the resource, which can be @WirelessDevice@ or
    -- @WirelessGateway@.
    resourceType :: Prelude.Text,
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourceLogLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'putResourceLogLevel_resourceIdentifier' - Undocumented member.
--
-- 'resourceType', 'putResourceLogLevel_resourceType' - The type of the resource, which can be @WirelessDevice@ or
-- @WirelessGateway@.
--
-- 'logLevel', 'putResourceLogLevel_logLevel' - Undocumented member.
newPutResourceLogLevel ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'logLevel'
  LogLevel ->
  PutResourceLogLevel
newPutResourceLogLevel
  pResourceIdentifier_
  pResourceType_
  pLogLevel_ =
    PutResourceLogLevel'
      { resourceIdentifier =
          pResourceIdentifier_,
        resourceType = pResourceType_,
        logLevel = pLogLevel_
      }

-- | Undocumented member.
putResourceLogLevel_resourceIdentifier :: Lens.Lens' PutResourceLogLevel Prelude.Text
putResourceLogLevel_resourceIdentifier = Lens.lens (\PutResourceLogLevel' {resourceIdentifier} -> resourceIdentifier) (\s@PutResourceLogLevel' {} a -> s {resourceIdentifier = a} :: PutResourceLogLevel)

-- | The type of the resource, which can be @WirelessDevice@ or
-- @WirelessGateway@.
putResourceLogLevel_resourceType :: Lens.Lens' PutResourceLogLevel Prelude.Text
putResourceLogLevel_resourceType = Lens.lens (\PutResourceLogLevel' {resourceType} -> resourceType) (\s@PutResourceLogLevel' {} a -> s {resourceType = a} :: PutResourceLogLevel)

-- | Undocumented member.
putResourceLogLevel_logLevel :: Lens.Lens' PutResourceLogLevel LogLevel
putResourceLogLevel_logLevel = Lens.lens (\PutResourceLogLevel' {logLevel} -> logLevel) (\s@PutResourceLogLevel' {} a -> s {logLevel = a} :: PutResourceLogLevel)

instance Core.AWSRequest PutResourceLogLevel where
  type
    AWSResponse PutResourceLogLevel =
      PutResourceLogLevelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutResourceLogLevelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourceLogLevel where
  hashWithSalt _salt PutResourceLogLevel' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData PutResourceLogLevel where
  rnf PutResourceLogLevel' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf logLevel

instance Core.ToHeaders PutResourceLogLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutResourceLogLevel where
  toJSON PutResourceLogLevel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LogLevel" Core..= logLevel)]
      )

instance Core.ToPath PutResourceLogLevel where
  toPath PutResourceLogLevel' {..} =
    Prelude.mconcat
      ["/log-levels/", Core.toBS resourceIdentifier]

instance Core.ToQuery PutResourceLogLevel where
  toQuery PutResourceLogLevel' {..} =
    Prelude.mconcat
      ["resourceType" Core.=: resourceType]

-- | /See:/ 'newPutResourceLogLevelResponse' smart constructor.
data PutResourceLogLevelResponse = PutResourceLogLevelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourceLogLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putResourceLogLevelResponse_httpStatus' - The response's http status code.
newPutResourceLogLevelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourceLogLevelResponse
newPutResourceLogLevelResponse pHttpStatus_ =
  PutResourceLogLevelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putResourceLogLevelResponse_httpStatus :: Lens.Lens' PutResourceLogLevelResponse Prelude.Int
putResourceLogLevelResponse_httpStatus = Lens.lens (\PutResourceLogLevelResponse' {httpStatus} -> httpStatus) (\s@PutResourceLogLevelResponse' {} a -> s {httpStatus = a} :: PutResourceLogLevelResponse)

instance Prelude.NFData PutResourceLogLevelResponse where
  rnf PutResourceLogLevelResponse' {..} =
    Prelude.rnf httpStatus
