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
-- Module      : Network.AWS.SES.CreateReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty receipt rule set.
--
-- For information about setting up receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptRuleSet
  ( -- * Creating a Request
    CreateReceiptRuleSet (..),
    newCreateReceiptRuleSet,

    -- * Request Lenses
    createReceiptRuleSet_ruleSetName,

    -- * Destructuring the Response
    CreateReceiptRuleSetResponse (..),
    newCreateReceiptRuleSetResponse,

    -- * Response Lenses
    createReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to create an empty receipt rule set. You use
-- receipt rule sets to receive email with Amazon SES. For more
-- information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateReceiptRuleSet' smart constructor.
data CreateReceiptRuleSet = CreateReceiptRuleSet'
  { -- | The name of the rule set to create. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Start and end with a letter or number.
    --
    -- -   Contain less than 64 characters.
    ruleSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'createReceiptRuleSet_ruleSetName' - The name of the rule set to create. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
newCreateReceiptRuleSet ::
  -- | 'ruleSetName'
  Core.Text ->
  CreateReceiptRuleSet
newCreateReceiptRuleSet pRuleSetName_ =
  CreateReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the rule set to create. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
createReceiptRuleSet_ruleSetName :: Lens.Lens' CreateReceiptRuleSet Core.Text
createReceiptRuleSet_ruleSetName = Lens.lens (\CreateReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@CreateReceiptRuleSet' {} a -> s {ruleSetName = a} :: CreateReceiptRuleSet)

instance Core.AWSRequest CreateReceiptRuleSet where
  type
    AWSResponse CreateReceiptRuleSet =
      CreateReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateReceiptRuleSetResult"
      ( \s h x ->
          CreateReceiptRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateReceiptRuleSet

instance Core.NFData CreateReceiptRuleSet

instance Core.ToHeaders CreateReceiptRuleSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateReceiptRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery CreateReceiptRuleSet where
  toQuery CreateReceiptRuleSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateReceiptRuleSet" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateReceiptRuleSetResponse' smart constructor.
data CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createReceiptRuleSetResponse_httpStatus' - The response's http status code.
newCreateReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateReceiptRuleSetResponse
newCreateReceiptRuleSetResponse pHttpStatus_ =
  CreateReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createReceiptRuleSetResponse_httpStatus :: Lens.Lens' CreateReceiptRuleSetResponse Core.Int
createReceiptRuleSetResponse_httpStatus = Lens.lens (\CreateReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@CreateReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: CreateReceiptRuleSetResponse)

instance Core.NFData CreateReceiptRuleSetResponse
