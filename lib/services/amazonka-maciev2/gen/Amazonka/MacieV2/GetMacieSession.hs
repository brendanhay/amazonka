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
-- Module      : Amazonka.MacieV2.GetMacieSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status and configuration settings for an Amazon
-- Macie account.
module Amazonka.MacieV2.GetMacieSession
  ( -- * Creating a Request
    GetMacieSession (..),
    newGetMacieSession,

    -- * Destructuring the Response
    GetMacieSessionResponse (..),
    newGetMacieSessionResponse,

    -- * Response Lenses
    getMacieSessionResponse_status,
    getMacieSessionResponse_serviceRole,
    getMacieSessionResponse_findingPublishingFrequency,
    getMacieSessionResponse_createdAt,
    getMacieSessionResponse_updatedAt,
    getMacieSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMacieSession' smart constructor.
data GetMacieSession = GetMacieSession'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMacieSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetMacieSession ::
  GetMacieSession
newGetMacieSession = GetMacieSession'

instance Core.AWSRequest GetMacieSession where
  type
    AWSResponse GetMacieSession =
      GetMacieSessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMacieSessionResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "serviceRole")
            Prelude.<*> (x Core..?> "findingPublishingFrequency")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "updatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMacieSession where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetMacieSession where
  rnf _ = ()

instance Core.ToHeaders GetMacieSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetMacieSession where
  toPath = Prelude.const "/macie"

instance Core.ToQuery GetMacieSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMacieSessionResponse' smart constructor.
data GetMacieSessionResponse = GetMacieSessionResponse'
  { -- | The current status of the Amazon Macie account. Possible values are:
    -- PAUSED, the account is enabled but all Macie activities are suspended
    -- (paused) for the account; and, ENABLED, the account is enabled and all
    -- Macie activities are enabled for the account.
    status :: Prelude.Maybe MacieStatus,
    -- | The Amazon Resource Name (ARN) of the service-linked role that allows
    -- Amazon Macie to monitor and analyze data in Amazon Web Services
    -- resources for the account.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The frequency with which Amazon Macie publishes updates to policy
    -- findings for the account. This includes publishing updates to Security
    -- Hub and Amazon EventBridge (formerly Amazon CloudWatch Events).
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | The date and time, in UTC and extended ISO 8601 format, when the Amazon
    -- Macie account was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time, in UTC and extended ISO 8601 format, of the most
    -- recent change to the status of the Amazon Macie account.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMacieSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMacieSessionResponse_status' - The current status of the Amazon Macie account. Possible values are:
-- PAUSED, the account is enabled but all Macie activities are suspended
-- (paused) for the account; and, ENABLED, the account is enabled and all
-- Macie activities are enabled for the account.
--
-- 'serviceRole', 'getMacieSessionResponse_serviceRole' - The Amazon Resource Name (ARN) of the service-linked role that allows
-- Amazon Macie to monitor and analyze data in Amazon Web Services
-- resources for the account.
--
-- 'findingPublishingFrequency', 'getMacieSessionResponse_findingPublishingFrequency' - The frequency with which Amazon Macie publishes updates to policy
-- findings for the account. This includes publishing updates to Security
-- Hub and Amazon EventBridge (formerly Amazon CloudWatch Events).
--
-- 'createdAt', 'getMacieSessionResponse_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the Amazon
-- Macie account was created.
--
-- 'updatedAt', 'getMacieSessionResponse_updatedAt' - The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the Amazon Macie account.
--
-- 'httpStatus', 'getMacieSessionResponse_httpStatus' - The response's http status code.
newGetMacieSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMacieSessionResponse
newGetMacieSessionResponse pHttpStatus_ =
  GetMacieSessionResponse'
    { status = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the Amazon Macie account. Possible values are:
-- PAUSED, the account is enabled but all Macie activities are suspended
-- (paused) for the account; and, ENABLED, the account is enabled and all
-- Macie activities are enabled for the account.
getMacieSessionResponse_status :: Lens.Lens' GetMacieSessionResponse (Prelude.Maybe MacieStatus)
getMacieSessionResponse_status = Lens.lens (\GetMacieSessionResponse' {status} -> status) (\s@GetMacieSessionResponse' {} a -> s {status = a} :: GetMacieSessionResponse)

-- | The Amazon Resource Name (ARN) of the service-linked role that allows
-- Amazon Macie to monitor and analyze data in Amazon Web Services
-- resources for the account.
getMacieSessionResponse_serviceRole :: Lens.Lens' GetMacieSessionResponse (Prelude.Maybe Prelude.Text)
getMacieSessionResponse_serviceRole = Lens.lens (\GetMacieSessionResponse' {serviceRole} -> serviceRole) (\s@GetMacieSessionResponse' {} a -> s {serviceRole = a} :: GetMacieSessionResponse)

-- | The frequency with which Amazon Macie publishes updates to policy
-- findings for the account. This includes publishing updates to Security
-- Hub and Amazon EventBridge (formerly Amazon CloudWatch Events).
getMacieSessionResponse_findingPublishingFrequency :: Lens.Lens' GetMacieSessionResponse (Prelude.Maybe FindingPublishingFrequency)
getMacieSessionResponse_findingPublishingFrequency = Lens.lens (\GetMacieSessionResponse' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@GetMacieSessionResponse' {} a -> s {findingPublishingFrequency = a} :: GetMacieSessionResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when the Amazon
-- Macie account was created.
getMacieSessionResponse_createdAt :: Lens.Lens' GetMacieSessionResponse (Prelude.Maybe Prelude.UTCTime)
getMacieSessionResponse_createdAt = Lens.lens (\GetMacieSessionResponse' {createdAt} -> createdAt) (\s@GetMacieSessionResponse' {} a -> s {createdAt = a} :: GetMacieSessionResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the Amazon Macie account.
getMacieSessionResponse_updatedAt :: Lens.Lens' GetMacieSessionResponse (Prelude.Maybe Prelude.UTCTime)
getMacieSessionResponse_updatedAt = Lens.lens (\GetMacieSessionResponse' {updatedAt} -> updatedAt) (\s@GetMacieSessionResponse' {} a -> s {updatedAt = a} :: GetMacieSessionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getMacieSessionResponse_httpStatus :: Lens.Lens' GetMacieSessionResponse Prelude.Int
getMacieSessionResponse_httpStatus = Lens.lens (\GetMacieSessionResponse' {httpStatus} -> httpStatus) (\s@GetMacieSessionResponse' {} a -> s {httpStatus = a} :: GetMacieSessionResponse)

instance Prelude.NFData GetMacieSessionResponse where
  rnf GetMacieSessionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf findingPublishingFrequency
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
