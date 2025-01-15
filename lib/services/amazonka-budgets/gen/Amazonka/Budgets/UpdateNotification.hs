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
-- Module      : Amazonka.Budgets.UpdateNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notification.
module Amazonka.Budgets.UpdateNotification
  ( -- * Creating a Request
    UpdateNotification (..),
    newUpdateNotification,

    -- * Request Lenses
    updateNotification_accountId,
    updateNotification_budgetName,
    updateNotification_oldNotification,
    updateNotification_newNotification,

    -- * Destructuring the Response
    UpdateNotificationResponse (..),
    newUpdateNotificationResponse,

    -- * Response Lenses
    updateNotificationResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of UpdateNotification
--
-- /See:/ 'newUpdateNotification' smart constructor.
data UpdateNotification = UpdateNotification'
  { -- | The @accountId@ that is associated with the budget whose notification
    -- you want to update.
    accountId :: Prelude.Text,
    -- | The name of the budget whose notification you want to update.
    budgetName :: Prelude.Text,
    -- | The previous notification that is associated with a budget.
    oldNotification :: Notification,
    -- | The updated notification to be associated with a budget.
    newNotification' :: Notification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updateNotification_accountId' - The @accountId@ that is associated with the budget whose notification
-- you want to update.
--
-- 'budgetName', 'updateNotification_budgetName' - The name of the budget whose notification you want to update.
--
-- 'oldNotification', 'updateNotification_oldNotification' - The previous notification that is associated with a budget.
--
-- 'newNotification'', 'updateNotification_newNotification' - The updated notification to be associated with a budget.
newUpdateNotification ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'oldNotification'
  Notification ->
  -- | 'newNotification''
  Notification ->
  UpdateNotification
newUpdateNotification
  pAccountId_
  pBudgetName_
  pOldNotification_
  pNewNotification_ =
    UpdateNotification'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        oldNotification = pOldNotification_,
        newNotification' = pNewNotification_
      }

-- | The @accountId@ that is associated with the budget whose notification
-- you want to update.
updateNotification_accountId :: Lens.Lens' UpdateNotification Prelude.Text
updateNotification_accountId = Lens.lens (\UpdateNotification' {accountId} -> accountId) (\s@UpdateNotification' {} a -> s {accountId = a} :: UpdateNotification)

-- | The name of the budget whose notification you want to update.
updateNotification_budgetName :: Lens.Lens' UpdateNotification Prelude.Text
updateNotification_budgetName = Lens.lens (\UpdateNotification' {budgetName} -> budgetName) (\s@UpdateNotification' {} a -> s {budgetName = a} :: UpdateNotification)

-- | The previous notification that is associated with a budget.
updateNotification_oldNotification :: Lens.Lens' UpdateNotification Notification
updateNotification_oldNotification = Lens.lens (\UpdateNotification' {oldNotification} -> oldNotification) (\s@UpdateNotification' {} a -> s {oldNotification = a} :: UpdateNotification)

-- | The updated notification to be associated with a budget.
updateNotification_newNotification :: Lens.Lens' UpdateNotification Notification
updateNotification_newNotification = Lens.lens (\UpdateNotification' {newNotification'} -> newNotification') (\s@UpdateNotification' {} a -> s {newNotification' = a} :: UpdateNotification)

instance Core.AWSRequest UpdateNotification where
  type
    AWSResponse UpdateNotification =
      UpdateNotificationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotification where
  hashWithSalt _salt UpdateNotification' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` oldNotification
      `Prelude.hashWithSalt` newNotification'

instance Prelude.NFData UpdateNotification where
  rnf UpdateNotification' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf budgetName `Prelude.seq`
        Prelude.rnf oldNotification `Prelude.seq`
          Prelude.rnf newNotification'

instance Data.ToHeaders UpdateNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.UpdateNotification" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotification where
  toJSON UpdateNotification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just
              ("OldNotification" Data..= oldNotification),
            Prelude.Just
              ("NewNotification" Data..= newNotification')
          ]
      )

instance Data.ToPath UpdateNotification where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNotification where
  toQuery = Prelude.const Prelude.mempty

-- | Response of UpdateNotification
--
-- /See:/ 'newUpdateNotificationResponse' smart constructor.
data UpdateNotificationResponse = UpdateNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotificationResponse_httpStatus' - The response's http status code.
newUpdateNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotificationResponse
newUpdateNotificationResponse pHttpStatus_ =
  UpdateNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotificationResponse_httpStatus :: Lens.Lens' UpdateNotificationResponse Prelude.Int
updateNotificationResponse_httpStatus = Lens.lens (\UpdateNotificationResponse' {httpStatus} -> httpStatus) (\s@UpdateNotificationResponse' {} a -> s {httpStatus = a} :: UpdateNotificationResponse)

instance Prelude.NFData UpdateNotificationResponse where
  rnf UpdateNotificationResponse' {..} =
    Prelude.rnf httpStatus
