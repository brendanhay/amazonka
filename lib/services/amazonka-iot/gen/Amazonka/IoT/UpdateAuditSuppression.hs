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
-- Module      : Amazonka.IoT.UpdateAuditSuppression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender audit suppression.
module Amazonka.IoT.UpdateAuditSuppression
  ( -- * Creating a Request
    UpdateAuditSuppression (..),
    newUpdateAuditSuppression,

    -- * Request Lenses
    updateAuditSuppression_description,
    updateAuditSuppression_expirationDate,
    updateAuditSuppression_suppressIndefinitely,
    updateAuditSuppression_checkName,
    updateAuditSuppression_resourceIdentifier,

    -- * Destructuring the Response
    UpdateAuditSuppressionResponse (..),
    newUpdateAuditSuppressionResponse,

    -- * Response Lenses
    updateAuditSuppressionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAuditSuppression' smart constructor.
data UpdateAuditSuppression = UpdateAuditSuppression'
  { -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | The expiration date (epoch timestamp in seconds) that you want the
    -- suppression to adhere to.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAuditSuppression_description' - The description of the audit suppression.
--
-- 'expirationDate', 'updateAuditSuppression_expirationDate' - The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
--
-- 'suppressIndefinitely', 'updateAuditSuppression_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'checkName', 'updateAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'updateAuditSuppression_resourceIdentifier' - Undocumented member.
newUpdateAuditSuppression ::
  -- | 'checkName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  UpdateAuditSuppression
newUpdateAuditSuppression
  pCheckName_
  pResourceIdentifier_ =
    UpdateAuditSuppression'
      { description =
          Prelude.Nothing,
        expirationDate = Prelude.Nothing,
        suppressIndefinitely = Prelude.Nothing,
        checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The description of the audit suppression.
updateAuditSuppression_description :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.Text)
updateAuditSuppression_description = Lens.lens (\UpdateAuditSuppression' {description} -> description) (\s@UpdateAuditSuppression' {} a -> s {description = a} :: UpdateAuditSuppression)

-- | The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
updateAuditSuppression_expirationDate :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.UTCTime)
updateAuditSuppression_expirationDate = Lens.lens (\UpdateAuditSuppression' {expirationDate} -> expirationDate) (\s@UpdateAuditSuppression' {} a -> s {expirationDate = a} :: UpdateAuditSuppression) Prelude.. Lens.mapping Data._Time

-- | Indicates whether a suppression should exist indefinitely or not.
updateAuditSuppression_suppressIndefinitely :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.Bool)
updateAuditSuppression_suppressIndefinitely = Lens.lens (\UpdateAuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@UpdateAuditSuppression' {} a -> s {suppressIndefinitely = a} :: UpdateAuditSuppression)

-- | Undocumented member.
updateAuditSuppression_checkName :: Lens.Lens' UpdateAuditSuppression Prelude.Text
updateAuditSuppression_checkName = Lens.lens (\UpdateAuditSuppression' {checkName} -> checkName) (\s@UpdateAuditSuppression' {} a -> s {checkName = a} :: UpdateAuditSuppression)

-- | Undocumented member.
updateAuditSuppression_resourceIdentifier :: Lens.Lens' UpdateAuditSuppression ResourceIdentifier
updateAuditSuppression_resourceIdentifier = Lens.lens (\UpdateAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@UpdateAuditSuppression' {} a -> s {resourceIdentifier = a} :: UpdateAuditSuppression)

instance Core.AWSRequest UpdateAuditSuppression where
  type
    AWSResponse UpdateAuditSuppression =
      UpdateAuditSuppressionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAuditSuppressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAuditSuppression where
  hashWithSalt _salt UpdateAuditSuppression' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` suppressIndefinitely
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData UpdateAuditSuppression where
  rnf UpdateAuditSuppression' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf suppressIndefinitely
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders UpdateAuditSuppression where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAuditSuppression where
  toJSON UpdateAuditSuppression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("expirationDate" Data..=)
              Prelude.<$> expirationDate,
            ("suppressIndefinitely" Data..=)
              Prelude.<$> suppressIndefinitely,
            Prelude.Just ("checkName" Data..= checkName),
            Prelude.Just
              ("resourceIdentifier" Data..= resourceIdentifier)
          ]
      )

instance Data.ToPath UpdateAuditSuppression where
  toPath = Prelude.const "/audit/suppressions/update"

instance Data.ToQuery UpdateAuditSuppression where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuditSuppressionResponse' smart constructor.
data UpdateAuditSuppressionResponse = UpdateAuditSuppressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuditSuppressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAuditSuppressionResponse_httpStatus' - The response's http status code.
newUpdateAuditSuppressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAuditSuppressionResponse
newUpdateAuditSuppressionResponse pHttpStatus_ =
  UpdateAuditSuppressionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAuditSuppressionResponse_httpStatus :: Lens.Lens' UpdateAuditSuppressionResponse Prelude.Int
updateAuditSuppressionResponse_httpStatus = Lens.lens (\UpdateAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@UpdateAuditSuppressionResponse' {} a -> s {httpStatus = a} :: UpdateAuditSuppressionResponse)

instance
  Prelude.NFData
    UpdateAuditSuppressionResponse
  where
  rnf UpdateAuditSuppressionResponse' {..} =
    Prelude.rnf httpStatus
