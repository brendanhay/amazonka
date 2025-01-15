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
-- Module      : Amazonka.MacieV2.UpdateMacieSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends or re-enables Amazon Macie, or updates the configuration
-- settings for a Macie account.
module Amazonka.MacieV2.UpdateMacieSession
  ( -- * Creating a Request
    UpdateMacieSession (..),
    newUpdateMacieSession,

    -- * Request Lenses
    updateMacieSession_findingPublishingFrequency,
    updateMacieSession_status,

    -- * Destructuring the Response
    UpdateMacieSessionResponse (..),
    newUpdateMacieSessionResponse,

    -- * Response Lenses
    updateMacieSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMacieSession' smart constructor.
data UpdateMacieSession = UpdateMacieSession'
  { -- | Specifies how often to publish updates to policy findings for the
    -- account. This includes publishing updates to Security Hub and Amazon
    -- EventBridge (formerly Amazon CloudWatch Events).
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | Specifies a new status for the account. Valid values are: ENABLED,
    -- resume all Amazon Macie activities for the account; and, PAUSED, suspend
    -- all Macie activities for the account.
    status :: Prelude.Maybe MacieStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMacieSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingPublishingFrequency', 'updateMacieSession_findingPublishingFrequency' - Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly Amazon CloudWatch Events).
--
-- 'status', 'updateMacieSession_status' - Specifies a new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
newUpdateMacieSession ::
  UpdateMacieSession
newUpdateMacieSession =
  UpdateMacieSession'
    { findingPublishingFrequency =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly Amazon CloudWatch Events).
updateMacieSession_findingPublishingFrequency :: Lens.Lens' UpdateMacieSession (Prelude.Maybe FindingPublishingFrequency)
updateMacieSession_findingPublishingFrequency = Lens.lens (\UpdateMacieSession' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@UpdateMacieSession' {} a -> s {findingPublishingFrequency = a} :: UpdateMacieSession)

-- | Specifies a new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
updateMacieSession_status :: Lens.Lens' UpdateMacieSession (Prelude.Maybe MacieStatus)
updateMacieSession_status = Lens.lens (\UpdateMacieSession' {status} -> status) (\s@UpdateMacieSession' {} a -> s {status = a} :: UpdateMacieSession)

instance Core.AWSRequest UpdateMacieSession where
  type
    AWSResponse UpdateMacieSession =
      UpdateMacieSessionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMacieSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMacieSession where
  hashWithSalt _salt UpdateMacieSession' {..} =
    _salt
      `Prelude.hashWithSalt` findingPublishingFrequency
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateMacieSession where
  rnf UpdateMacieSession' {..} =
    Prelude.rnf findingPublishingFrequency `Prelude.seq`
      Prelude.rnf status

instance Data.ToHeaders UpdateMacieSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMacieSession where
  toJSON UpdateMacieSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("findingPublishingFrequency" Data..=)
              Prelude.<$> findingPublishingFrequency,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath UpdateMacieSession where
  toPath = Prelude.const "/macie"

instance Data.ToQuery UpdateMacieSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMacieSessionResponse' smart constructor.
data UpdateMacieSessionResponse = UpdateMacieSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMacieSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMacieSessionResponse_httpStatus' - The response's http status code.
newUpdateMacieSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMacieSessionResponse
newUpdateMacieSessionResponse pHttpStatus_ =
  UpdateMacieSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateMacieSessionResponse_httpStatus :: Lens.Lens' UpdateMacieSessionResponse Prelude.Int
updateMacieSessionResponse_httpStatus = Lens.lens (\UpdateMacieSessionResponse' {httpStatus} -> httpStatus) (\s@UpdateMacieSessionResponse' {} a -> s {httpStatus = a} :: UpdateMacieSessionResponse)

instance Prelude.NFData UpdateMacieSessionResponse where
  rnf UpdateMacieSessionResponse' {..} =
    Prelude.rnf httpStatus
