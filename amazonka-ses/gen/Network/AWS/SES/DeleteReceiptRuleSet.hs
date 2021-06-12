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
-- Module      : Network.AWS.SES.DeleteReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.DeleteReceiptRuleSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete a receipt rule set and all of the receipt
-- rules it contains. You use receipt rule sets to receive email with
-- Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteReceiptRuleSet' smart constructor.
data DeleteReceiptRuleSet = DeleteReceiptRuleSet'
  { -- | The name of the receipt rule set to delete.
    ruleSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteReceiptRuleSet
newDeleteReceiptRuleSet pRuleSetName_ =
  DeleteReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the receipt rule set to delete.
deleteReceiptRuleSet_ruleSetName :: Lens.Lens' DeleteReceiptRuleSet Core.Text
deleteReceiptRuleSet_ruleSetName = Lens.lens (\DeleteReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@DeleteReceiptRuleSet' {} a -> s {ruleSetName = a} :: DeleteReceiptRuleSet)

instance Core.AWSRequest DeleteReceiptRuleSet where
  type
    AWSResponse DeleteReceiptRuleSet =
      DeleteReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteReceiptRuleSetResult"
      ( \s h x ->
          DeleteReceiptRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReceiptRuleSet

instance Core.NFData DeleteReceiptRuleSet

instance Core.ToHeaders DeleteReceiptRuleSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteReceiptRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReceiptRuleSet where
  toQuery DeleteReceiptRuleSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteReceiptRuleSet" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteReceiptRuleSetResponse' smart constructor.
data DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteReceiptRuleSetResponse
newDeleteReceiptRuleSetResponse pHttpStatus_ =
  DeleteReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReceiptRuleSetResponse_httpStatus :: Lens.Lens' DeleteReceiptRuleSetResponse Core.Int
deleteReceiptRuleSetResponse_httpStatus = Lens.lens (\DeleteReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DeleteReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DeleteReceiptRuleSetResponse)

instance Core.NFData DeleteReceiptRuleSetResponse
