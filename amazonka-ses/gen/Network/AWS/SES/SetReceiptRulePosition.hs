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
-- Module      : Network.AWS.SES.SetReceiptRulePosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the position of the specified receipt rule in the receipt rule set.
--
-- For information about managing receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetReceiptRulePosition
  ( -- * Creating a Request
    SetReceiptRulePosition (..),
    newSetReceiptRulePosition,

    -- * Request Lenses
    setReceiptRulePosition_after,
    setReceiptRulePosition_ruleSetName,
    setReceiptRulePosition_ruleName,

    -- * Destructuring the Response
    SetReceiptRulePositionResponse (..),
    newSetReceiptRulePositionResponse,

    -- * Response Lenses
    setReceiptRulePositionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to set the position of a receipt rule in a receipt
-- rule set. You use receipt rule sets to receive email with Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetReceiptRulePosition' smart constructor.
data SetReceiptRulePosition = SetReceiptRulePosition'
  { -- | The name of the receipt rule after which to place the specified receipt
    -- rule.
    after :: Core.Maybe Core.Text,
    -- | The name of the receipt rule set that contains the receipt rule to
    -- reposition.
    ruleSetName :: Core.Text,
    -- | The name of the receipt rule to reposition.
    ruleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetReceiptRulePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'after', 'setReceiptRulePosition_after' - The name of the receipt rule after which to place the specified receipt
-- rule.
--
-- 'ruleSetName', 'setReceiptRulePosition_ruleSetName' - The name of the receipt rule set that contains the receipt rule to
-- reposition.
--
-- 'ruleName', 'setReceiptRulePosition_ruleName' - The name of the receipt rule to reposition.
newSetReceiptRulePosition ::
  -- | 'ruleSetName'
  Core.Text ->
  -- | 'ruleName'
  Core.Text ->
  SetReceiptRulePosition
newSetReceiptRulePosition pRuleSetName_ pRuleName_ =
  SetReceiptRulePosition'
    { after = Core.Nothing,
      ruleSetName = pRuleSetName_,
      ruleName = pRuleName_
    }

-- | The name of the receipt rule after which to place the specified receipt
-- rule.
setReceiptRulePosition_after :: Lens.Lens' SetReceiptRulePosition (Core.Maybe Core.Text)
setReceiptRulePosition_after = Lens.lens (\SetReceiptRulePosition' {after} -> after) (\s@SetReceiptRulePosition' {} a -> s {after = a} :: SetReceiptRulePosition)

-- | The name of the receipt rule set that contains the receipt rule to
-- reposition.
setReceiptRulePosition_ruleSetName :: Lens.Lens' SetReceiptRulePosition Core.Text
setReceiptRulePosition_ruleSetName = Lens.lens (\SetReceiptRulePosition' {ruleSetName} -> ruleSetName) (\s@SetReceiptRulePosition' {} a -> s {ruleSetName = a} :: SetReceiptRulePosition)

-- | The name of the receipt rule to reposition.
setReceiptRulePosition_ruleName :: Lens.Lens' SetReceiptRulePosition Core.Text
setReceiptRulePosition_ruleName = Lens.lens (\SetReceiptRulePosition' {ruleName} -> ruleName) (\s@SetReceiptRulePosition' {} a -> s {ruleName = a} :: SetReceiptRulePosition)

instance Core.AWSRequest SetReceiptRulePosition where
  type
    AWSResponse SetReceiptRulePosition =
      SetReceiptRulePositionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetReceiptRulePositionResult"
      ( \s h x ->
          SetReceiptRulePositionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetReceiptRulePosition

instance Core.NFData SetReceiptRulePosition

instance Core.ToHeaders SetReceiptRulePosition where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetReceiptRulePosition where
  toPath = Core.const "/"

instance Core.ToQuery SetReceiptRulePosition where
  toQuery SetReceiptRulePosition' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetReceiptRulePosition" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "After" Core.=: after,
        "RuleSetName" Core.=: ruleSetName,
        "RuleName" Core.=: ruleName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetReceiptRulePositionResponse' smart constructor.
data SetReceiptRulePositionResponse = SetReceiptRulePositionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetReceiptRulePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setReceiptRulePositionResponse_httpStatus' - The response's http status code.
newSetReceiptRulePositionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetReceiptRulePositionResponse
newSetReceiptRulePositionResponse pHttpStatus_ =
  SetReceiptRulePositionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setReceiptRulePositionResponse_httpStatus :: Lens.Lens' SetReceiptRulePositionResponse Core.Int
setReceiptRulePositionResponse_httpStatus = Lens.lens (\SetReceiptRulePositionResponse' {httpStatus} -> httpStatus) (\s@SetReceiptRulePositionResponse' {} a -> s {httpStatus = a} :: SetReceiptRulePositionResponse)

instance Core.NFData SetReceiptRulePositionResponse
