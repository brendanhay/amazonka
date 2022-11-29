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
-- Module      : Amazonka.SES.DeleteReceiptRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule set and all of the receipt rules it
-- contains.
--
-- The currently active rule set cannot be deleted.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteReceiptRuleSet
  ( -- * Creating a Request
    DeleteReceiptRuleSet (..),
    newDeleteReceiptRuleSet,

    -- * Request Lenses
    deleteReceiptRuleSet_ruleSetName,

    -- * Destructuring the Response
    DeleteReceiptRuleSetResponse (..),
    newDeleteReceiptRuleSetResponse,

    -- * Response Lenses
    deleteReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete a receipt rule set and all of the receipt
-- rules it contains. You use receipt rule sets to receive email with
-- Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteReceiptRuleSet' smart constructor.
data DeleteReceiptRuleSet = DeleteReceiptRuleSet'
  { -- | The name of the receipt rule set to delete.
    ruleSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'deleteReceiptRuleSet_ruleSetName' - The name of the receipt rule set to delete.
newDeleteReceiptRuleSet ::
  -- | 'ruleSetName'
  Prelude.Text ->
  DeleteReceiptRuleSet
newDeleteReceiptRuleSet pRuleSetName_ =
  DeleteReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the receipt rule set to delete.
deleteReceiptRuleSet_ruleSetName :: Lens.Lens' DeleteReceiptRuleSet Prelude.Text
deleteReceiptRuleSet_ruleSetName = Lens.lens (\DeleteReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@DeleteReceiptRuleSet' {} a -> s {ruleSetName = a} :: DeleteReceiptRuleSet)

instance Core.AWSRequest DeleteReceiptRuleSet where
  type
    AWSResponse DeleteReceiptRuleSet =
      DeleteReceiptRuleSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteReceiptRuleSetResult"
      ( \s h x ->
          DeleteReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReceiptRuleSet where
  hashWithSalt _salt DeleteReceiptRuleSet' {..} =
    _salt `Prelude.hashWithSalt` ruleSetName

instance Prelude.NFData DeleteReceiptRuleSet where
  rnf DeleteReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName

instance Core.ToHeaders DeleteReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteReceiptRuleSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteReceiptRuleSet where
  toQuery DeleteReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteReceiptRuleSetResponse' smart constructor.
data DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReceiptRuleSetResponse_httpStatus' - The response's http status code.
newDeleteReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReceiptRuleSetResponse
newDeleteReceiptRuleSetResponse pHttpStatus_ =
  DeleteReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReceiptRuleSetResponse_httpStatus :: Lens.Lens' DeleteReceiptRuleSetResponse Prelude.Int
deleteReceiptRuleSetResponse_httpStatus = Lens.lens (\DeleteReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DeleteReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DeleteReceiptRuleSetResponse)

instance Prelude.NFData DeleteReceiptRuleSetResponse where
  rnf DeleteReceiptRuleSetResponse' {..} =
    Prelude.rnf httpStatus
