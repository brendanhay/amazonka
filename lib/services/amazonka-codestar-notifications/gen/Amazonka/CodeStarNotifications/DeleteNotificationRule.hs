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
-- Module      : Amazonka.CodeStarNotifications.DeleteNotificationRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notification rule for a resource.
module Amazonka.CodeStarNotifications.DeleteNotificationRule
  ( -- * Creating a Request
    DeleteNotificationRule (..),
    newDeleteNotificationRule,

    -- * Request Lenses
    deleteNotificationRule_arn,

    -- * Destructuring the Response
    DeleteNotificationRuleResponse (..),
    newDeleteNotificationRuleResponse,

    -- * Response Lenses
    deleteNotificationRuleResponse_arn,
    deleteNotificationRuleResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNotificationRule' smart constructor.
data DeleteNotificationRule = DeleteNotificationRule'
  { -- | The Amazon Resource Name (ARN) of the notification rule you want to
    -- delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteNotificationRule_arn' - The Amazon Resource Name (ARN) of the notification rule you want to
-- delete.
newDeleteNotificationRule ::
  -- | 'arn'
  Prelude.Text ->
  DeleteNotificationRule
newDeleteNotificationRule pArn_ =
  DeleteNotificationRule' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the notification rule you want to
-- delete.
deleteNotificationRule_arn :: Lens.Lens' DeleteNotificationRule Prelude.Text
deleteNotificationRule_arn = Lens.lens (\DeleteNotificationRule' {arn} -> arn) (\s@DeleteNotificationRule' {} a -> s {arn = a} :: DeleteNotificationRule)

instance Core.AWSRequest DeleteNotificationRule where
  type
    AWSResponse DeleteNotificationRule =
      DeleteNotificationRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNotificationRuleResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNotificationRule where
  hashWithSalt _salt DeleteNotificationRule' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteNotificationRule where
  rnf DeleteNotificationRule' {..} = Prelude.rnf arn

instance Core.ToHeaders DeleteNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteNotificationRule where
  toJSON DeleteNotificationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Core..= arn)]
      )

instance Core.ToPath DeleteNotificationRule where
  toPath = Prelude.const "/deleteNotificationRule"

instance Core.ToQuery DeleteNotificationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotificationRuleResponse' smart constructor.
data DeleteNotificationRuleResponse = DeleteNotificationRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted notification rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteNotificationRuleResponse_arn' - The Amazon Resource Name (ARN) of the deleted notification rule.
--
-- 'httpStatus', 'deleteNotificationRuleResponse_httpStatus' - The response's http status code.
newDeleteNotificationRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNotificationRuleResponse
newDeleteNotificationRuleResponse pHttpStatus_ =
  DeleteNotificationRuleResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted notification rule.
deleteNotificationRuleResponse_arn :: Lens.Lens' DeleteNotificationRuleResponse (Prelude.Maybe Prelude.Text)
deleteNotificationRuleResponse_arn = Lens.lens (\DeleteNotificationRuleResponse' {arn} -> arn) (\s@DeleteNotificationRuleResponse' {} a -> s {arn = a} :: DeleteNotificationRuleResponse)

-- | The response's http status code.
deleteNotificationRuleResponse_httpStatus :: Lens.Lens' DeleteNotificationRuleResponse Prelude.Int
deleteNotificationRuleResponse_httpStatus = Lens.lens (\DeleteNotificationRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteNotificationRuleResponse' {} a -> s {httpStatus = a} :: DeleteNotificationRuleResponse)

instance
  Prelude.NFData
    DeleteNotificationRuleResponse
  where
  rnf DeleteNotificationRuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
