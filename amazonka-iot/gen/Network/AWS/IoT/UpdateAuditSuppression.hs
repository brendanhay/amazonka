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
-- Module      : Network.AWS.IoT.UpdateAuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender audit suppression.
module Network.AWS.IoT.UpdateAuditSuppression
  ( -- * Creating a Request
    UpdateAuditSuppression (..),
    newUpdateAuditSuppression,

    -- * Request Lenses
    updateAuditSuppression_expirationDate,
    updateAuditSuppression_description,
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAuditSuppression' smart constructor.
data UpdateAuditSuppression = UpdateAuditSuppression'
  { -- | The expiration date (epoch timestamp in seconds) that you want the
    -- suppression to adhere to.
    expirationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDate', 'updateAuditSuppression_expirationDate' - The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
--
-- 'description', 'updateAuditSuppression_description' - The description of the audit suppression.
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
      { expirationDate =
          Prelude.Nothing,
        description = Prelude.Nothing,
        suppressIndefinitely = Prelude.Nothing,
        checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
updateAuditSuppression_expirationDate :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.UTCTime)
updateAuditSuppression_expirationDate = Lens.lens (\UpdateAuditSuppression' {expirationDate} -> expirationDate) (\s@UpdateAuditSuppression' {} a -> s {expirationDate = a} :: UpdateAuditSuppression) Prelude.. Lens.mapping Prelude._Time

-- | The description of the audit suppression.
updateAuditSuppression_description :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.Text)
updateAuditSuppression_description = Lens.lens (\UpdateAuditSuppression' {description} -> description) (\s@UpdateAuditSuppression' {} a -> s {description = a} :: UpdateAuditSuppression)

-- | Indicates whether a suppression should exist indefinitely or not.
updateAuditSuppression_suppressIndefinitely :: Lens.Lens' UpdateAuditSuppression (Prelude.Maybe Prelude.Bool)
updateAuditSuppression_suppressIndefinitely = Lens.lens (\UpdateAuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@UpdateAuditSuppression' {} a -> s {suppressIndefinitely = a} :: UpdateAuditSuppression)

-- | Undocumented member.
updateAuditSuppression_checkName :: Lens.Lens' UpdateAuditSuppression Prelude.Text
updateAuditSuppression_checkName = Lens.lens (\UpdateAuditSuppression' {checkName} -> checkName) (\s@UpdateAuditSuppression' {} a -> s {checkName = a} :: UpdateAuditSuppression)

-- | Undocumented member.
updateAuditSuppression_resourceIdentifier :: Lens.Lens' UpdateAuditSuppression ResourceIdentifier
updateAuditSuppression_resourceIdentifier = Lens.lens (\UpdateAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@UpdateAuditSuppression' {} a -> s {resourceIdentifier = a} :: UpdateAuditSuppression)

instance Prelude.AWSRequest UpdateAuditSuppression where
  type
    Rs UpdateAuditSuppression =
      UpdateAuditSuppressionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAuditSuppressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAuditSuppression

instance Prelude.NFData UpdateAuditSuppression

instance Prelude.ToHeaders UpdateAuditSuppression where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateAuditSuppression where
  toJSON UpdateAuditSuppression' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("expirationDate" Prelude..=)
              Prelude.<$> expirationDate,
            ("description" Prelude..=) Prelude.<$> description,
            ("suppressIndefinitely" Prelude..=)
              Prelude.<$> suppressIndefinitely,
            Prelude.Just ("checkName" Prelude..= checkName),
            Prelude.Just
              ( "resourceIdentifier"
                  Prelude..= resourceIdentifier
              )
          ]
      )

instance Prelude.ToPath UpdateAuditSuppression where
  toPath = Prelude.const "/audit/suppressions/update"

instance Prelude.ToQuery UpdateAuditSuppression where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuditSuppressionResponse' smart constructor.
data UpdateAuditSuppressionResponse = UpdateAuditSuppressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
