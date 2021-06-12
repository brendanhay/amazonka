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
-- Module      : Network.AWS.SES.SetActiveReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified receipt rule set as the active receipt rule set.
--
-- To disable your email-receiving through Amazon SES completely, you can
-- call this API with RuleSetName set to null.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetActiveReceiptRuleSet
  ( -- * Creating a Request
    SetActiveReceiptRuleSet (..),
    newSetActiveReceiptRuleSet,

    -- * Request Lenses
    setActiveReceiptRuleSet_ruleSetName,

    -- * Destructuring the Response
    SetActiveReceiptRuleSetResponse (..),
    newSetActiveReceiptRuleSetResponse,

    -- * Response Lenses
    setActiveReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to set a receipt rule set as the active receipt
-- rule set. You use receipt rule sets to receive email with Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetActiveReceiptRuleSet' smart constructor.
data SetActiveReceiptRuleSet = SetActiveReceiptRuleSet'
  { -- | The name of the receipt rule set to make active. Setting this value to
    -- null disables all email receiving.
    ruleSetName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetActiveReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'setActiveReceiptRuleSet_ruleSetName' - The name of the receipt rule set to make active. Setting this value to
-- null disables all email receiving.
newSetActiveReceiptRuleSet ::
  SetActiveReceiptRuleSet
newSetActiveReceiptRuleSet =
  SetActiveReceiptRuleSet'
    { ruleSetName =
        Core.Nothing
    }

-- | The name of the receipt rule set to make active. Setting this value to
-- null disables all email receiving.
setActiveReceiptRuleSet_ruleSetName :: Lens.Lens' SetActiveReceiptRuleSet (Core.Maybe Core.Text)
setActiveReceiptRuleSet_ruleSetName = Lens.lens (\SetActiveReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@SetActiveReceiptRuleSet' {} a -> s {ruleSetName = a} :: SetActiveReceiptRuleSet)

instance Core.AWSRequest SetActiveReceiptRuleSet where
  type
    AWSResponse SetActiveReceiptRuleSet =
      SetActiveReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetActiveReceiptRuleSetResult"
      ( \s h x ->
          SetActiveReceiptRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetActiveReceiptRuleSet

instance Core.NFData SetActiveReceiptRuleSet

instance Core.ToHeaders SetActiveReceiptRuleSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetActiveReceiptRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery SetActiveReceiptRuleSet where
  toQuery SetActiveReceiptRuleSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetActiveReceiptRuleSet" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetActiveReceiptRuleSetResponse' smart constructor.
data SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetActiveReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setActiveReceiptRuleSetResponse_httpStatus' - The response's http status code.
newSetActiveReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetActiveReceiptRuleSetResponse
newSetActiveReceiptRuleSetResponse pHttpStatus_ =
  SetActiveReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setActiveReceiptRuleSetResponse_httpStatus :: Lens.Lens' SetActiveReceiptRuleSetResponse Core.Int
setActiveReceiptRuleSetResponse_httpStatus = Lens.lens (\SetActiveReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@SetActiveReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: SetActiveReceiptRuleSetResponse)

instance Core.NFData SetActiveReceiptRuleSetResponse
