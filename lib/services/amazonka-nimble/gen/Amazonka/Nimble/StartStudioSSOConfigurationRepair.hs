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
-- Module      : Amazonka.Nimble.StartStudioSSOConfigurationRepair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Repairs the IAM Identity Center configuration for a given studio.
--
-- If the studio has a valid IAM Identity Center configuration currently
-- associated with it, this operation will fail with a validation error.
--
-- If the studio does not have a valid IAM Identity Center configuration
-- currently associated with it, then a new IAM Identity Center application
-- is created for the studio and the studio is changed to the @READY@
-- state.
--
-- After the IAM Identity Center application is repaired, you must use the
-- Amazon Nimble Studio console to add administrators and users to your
-- studio.
module Amazonka.Nimble.StartStudioSSOConfigurationRepair
  ( -- * Creating a Request
    StartStudioSSOConfigurationRepair (..),
    newStartStudioSSOConfigurationRepair,

    -- * Request Lenses
    startStudioSSOConfigurationRepair_clientToken,
    startStudioSSOConfigurationRepair_studioId,

    -- * Destructuring the Response
    StartStudioSSOConfigurationRepairResponse (..),
    newStartStudioSSOConfigurationRepairResponse,

    -- * Response Lenses
    startStudioSSOConfigurationRepairResponse_httpStatus,
    startStudioSSOConfigurationRepairResponse_studio,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartStudioSSOConfigurationRepair' smart constructor.
data StartStudioSSOConfigurationRepair = StartStudioSSOConfigurationRepair'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStudioSSOConfigurationRepair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startStudioSSOConfigurationRepair_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'studioId', 'startStudioSSOConfigurationRepair_studioId' - The studio ID.
newStartStudioSSOConfigurationRepair ::
  -- | 'studioId'
  Prelude.Text ->
  StartStudioSSOConfigurationRepair
newStartStudioSSOConfigurationRepair pStudioId_ =
  StartStudioSSOConfigurationRepair'
    { clientToken =
        Prelude.Nothing,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
startStudioSSOConfigurationRepair_clientToken :: Lens.Lens' StartStudioSSOConfigurationRepair (Prelude.Maybe Prelude.Text)
startStudioSSOConfigurationRepair_clientToken = Lens.lens (\StartStudioSSOConfigurationRepair' {clientToken} -> clientToken) (\s@StartStudioSSOConfigurationRepair' {} a -> s {clientToken = a} :: StartStudioSSOConfigurationRepair)

-- | The studio ID.
startStudioSSOConfigurationRepair_studioId :: Lens.Lens' StartStudioSSOConfigurationRepair Prelude.Text
startStudioSSOConfigurationRepair_studioId = Lens.lens (\StartStudioSSOConfigurationRepair' {studioId} -> studioId) (\s@StartStudioSSOConfigurationRepair' {} a -> s {studioId = a} :: StartStudioSSOConfigurationRepair)

instance
  Core.AWSRequest
    StartStudioSSOConfigurationRepair
  where
  type
    AWSResponse StartStudioSSOConfigurationRepair =
      StartStudioSSOConfigurationRepairResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartStudioSSOConfigurationRepairResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "studio")
      )

instance
  Prelude.Hashable
    StartStudioSSOConfigurationRepair
  where
  hashWithSalt
    _salt
    StartStudioSSOConfigurationRepair' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` studioId

instance
  Prelude.NFData
    StartStudioSSOConfigurationRepair
  where
  rnf StartStudioSSOConfigurationRepair' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioId

instance
  Data.ToHeaders
    StartStudioSSOConfigurationRepair
  where
  toHeaders StartStudioSSOConfigurationRepair' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Data.ToJSON
    StartStudioSSOConfigurationRepair
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    StartStudioSSOConfigurationRepair
  where
  toPath StartStudioSSOConfigurationRepair' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/sso-configuration"
      ]

instance
  Data.ToQuery
    StartStudioSSOConfigurationRepair
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStudioSSOConfigurationRepairResponse' smart constructor.
data StartStudioSSOConfigurationRepairResponse = StartStudioSSOConfigurationRepairResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a studio.
    studio :: Studio
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStudioSSOConfigurationRepairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startStudioSSOConfigurationRepairResponse_httpStatus' - The response's http status code.
--
-- 'studio', 'startStudioSSOConfigurationRepairResponse_studio' - Information about a studio.
newStartStudioSSOConfigurationRepairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'studio'
  Studio ->
  StartStudioSSOConfigurationRepairResponse
newStartStudioSSOConfigurationRepairResponse
  pHttpStatus_
  pStudio_ =
    StartStudioSSOConfigurationRepairResponse'
      { httpStatus =
          pHttpStatus_,
        studio = pStudio_
      }

-- | The response's http status code.
startStudioSSOConfigurationRepairResponse_httpStatus :: Lens.Lens' StartStudioSSOConfigurationRepairResponse Prelude.Int
startStudioSSOConfigurationRepairResponse_httpStatus = Lens.lens (\StartStudioSSOConfigurationRepairResponse' {httpStatus} -> httpStatus) (\s@StartStudioSSOConfigurationRepairResponse' {} a -> s {httpStatus = a} :: StartStudioSSOConfigurationRepairResponse)

-- | Information about a studio.
startStudioSSOConfigurationRepairResponse_studio :: Lens.Lens' StartStudioSSOConfigurationRepairResponse Studio
startStudioSSOConfigurationRepairResponse_studio = Lens.lens (\StartStudioSSOConfigurationRepairResponse' {studio} -> studio) (\s@StartStudioSSOConfigurationRepairResponse' {} a -> s {studio = a} :: StartStudioSSOConfigurationRepairResponse)

instance
  Prelude.NFData
    StartStudioSSOConfigurationRepairResponse
  where
  rnf StartStudioSSOConfigurationRepairResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf studio
