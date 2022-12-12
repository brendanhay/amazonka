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
-- Module      : Amazonka.MacieV2.EnableMacie
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Amazon Macie and specifies the configuration settings for a
-- Macie account.
module Amazonka.MacieV2.EnableMacie
  ( -- * Creating a Request
    EnableMacie (..),
    newEnableMacie,

    -- * Request Lenses
    enableMacie_clientToken,
    enableMacie_findingPublishingFrequency,
    enableMacie_status,

    -- * Destructuring the Response
    EnableMacieResponse (..),
    newEnableMacieResponse,

    -- * Response Lenses
    enableMacieResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableMacie' smart constructor.
data EnableMacie = EnableMacie'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies how often to publish updates to policy findings for the
    -- account. This includes publishing updates to Security Hub and Amazon
    -- EventBridge (formerly Amazon CloudWatch Events).
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | Specifies the new status for the account. To enable Amazon Macie and
    -- start all Macie activities for the account, set this value to ENABLED.
    status :: Prelude.Maybe MacieStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMacie' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'enableMacie_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'findingPublishingFrequency', 'enableMacie_findingPublishingFrequency' - Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly Amazon CloudWatch Events).
--
-- 'status', 'enableMacie_status' - Specifies the new status for the account. To enable Amazon Macie and
-- start all Macie activities for the account, set this value to ENABLED.
newEnableMacie ::
  EnableMacie
newEnableMacie =
  EnableMacie'
    { clientToken = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
enableMacie_clientToken :: Lens.Lens' EnableMacie (Prelude.Maybe Prelude.Text)
enableMacie_clientToken = Lens.lens (\EnableMacie' {clientToken} -> clientToken) (\s@EnableMacie' {} a -> s {clientToken = a} :: EnableMacie)

-- | Specifies how often to publish updates to policy findings for the
-- account. This includes publishing updates to Security Hub and Amazon
-- EventBridge (formerly Amazon CloudWatch Events).
enableMacie_findingPublishingFrequency :: Lens.Lens' EnableMacie (Prelude.Maybe FindingPublishingFrequency)
enableMacie_findingPublishingFrequency = Lens.lens (\EnableMacie' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@EnableMacie' {} a -> s {findingPublishingFrequency = a} :: EnableMacie)

-- | Specifies the new status for the account. To enable Amazon Macie and
-- start all Macie activities for the account, set this value to ENABLED.
enableMacie_status :: Lens.Lens' EnableMacie (Prelude.Maybe MacieStatus)
enableMacie_status = Lens.lens (\EnableMacie' {status} -> status) (\s@EnableMacie' {} a -> s {status = a} :: EnableMacie)

instance Core.AWSRequest EnableMacie where
  type AWSResponse EnableMacie = EnableMacieResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableMacieResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableMacie where
  hashWithSalt _salt EnableMacie' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` findingPublishingFrequency
      `Prelude.hashWithSalt` status

instance Prelude.NFData EnableMacie where
  rnf EnableMacie' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf findingPublishingFrequency
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders EnableMacie where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableMacie where
  toJSON EnableMacie' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("findingPublishingFrequency" Data..=)
              Prelude.<$> findingPublishingFrequency,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath EnableMacie where
  toPath = Prelude.const "/macie"

instance Data.ToQuery EnableMacie where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableMacieResponse' smart constructor.
data EnableMacieResponse = EnableMacieResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMacieResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableMacieResponse_httpStatus' - The response's http status code.
newEnableMacieResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableMacieResponse
newEnableMacieResponse pHttpStatus_ =
  EnableMacieResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableMacieResponse_httpStatus :: Lens.Lens' EnableMacieResponse Prelude.Int
enableMacieResponse_httpStatus = Lens.lens (\EnableMacieResponse' {httpStatus} -> httpStatus) (\s@EnableMacieResponse' {} a -> s {httpStatus = a} :: EnableMacieResponse)

instance Prelude.NFData EnableMacieResponse where
  rnf EnableMacieResponse' {..} = Prelude.rnf httpStatus
