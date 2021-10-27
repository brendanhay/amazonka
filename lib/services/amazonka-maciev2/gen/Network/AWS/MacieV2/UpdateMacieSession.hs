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
-- Module      : Network.AWS.MacieV2.UpdateMacieSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends or re-enables an Amazon Macie account, or updates the
-- configuration settings for a Macie account.
module Network.AWS.MacieV2.UpdateMacieSession
  ( -- * Creating a Request
    UpdateMacieSession (..),
    newUpdateMacieSession,

    -- * Request Lenses
    updateMacieSession_status,
    updateMacieSession_findingPublishingFrequency,

    -- * Destructuring the Response
    UpdateMacieSessionResponse (..),
    newUpdateMacieSessionResponse,

    -- * Response Lenses
    updateMacieSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMacieSession' smart constructor.
data UpdateMacieSession = UpdateMacieSession'
  { -- | Specifies a new status for the account. Valid values are: ENABLED,
    -- resume all Amazon Macie activities for the account; and, PAUSED, suspend
    -- all Macie activities for the account.
    status :: Prelude.Maybe MacieStatus,
    -- | Specifies how often to publish updates to policy findings for the
    -- account. This includes publishing updates to Security Hub and Amazon
    -- EventBridge (formerly called Amazon CloudWatch Events).
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency
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
-- 'status', 'updateMacieSession_status' - Specifies a new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
--
-- 'findingPublishingFrequency', 'updateMacieSession_findingPublishingFrequency' - Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly called Amazon CloudWatch Events).
newUpdateMacieSession ::
  UpdateMacieSession
newUpdateMacieSession =
  UpdateMacieSession'
    { status = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing
    }

-- | Specifies a new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
updateMacieSession_status :: Lens.Lens' UpdateMacieSession (Prelude.Maybe MacieStatus)
updateMacieSession_status = Lens.lens (\UpdateMacieSession' {status} -> status) (\s@UpdateMacieSession' {} a -> s {status = a} :: UpdateMacieSession)

-- | Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly called Amazon CloudWatch Events).
updateMacieSession_findingPublishingFrequency :: Lens.Lens' UpdateMacieSession (Prelude.Maybe FindingPublishingFrequency)
updateMacieSession_findingPublishingFrequency = Lens.lens (\UpdateMacieSession' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@UpdateMacieSession' {} a -> s {findingPublishingFrequency = a} :: UpdateMacieSession)

instance Core.AWSRequest UpdateMacieSession where
  type
    AWSResponse UpdateMacieSession =
      UpdateMacieSessionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMacieSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMacieSession

instance Prelude.NFData UpdateMacieSession

instance Core.ToHeaders UpdateMacieSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMacieSession where
  toJSON UpdateMacieSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("findingPublishingFrequency" Core..=)
              Prelude.<$> findingPublishingFrequency
          ]
      )

instance Core.ToPath UpdateMacieSession where
  toPath = Prelude.const "/macie"

instance Core.ToQuery UpdateMacieSession where
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

instance Prelude.NFData UpdateMacieSessionResponse
