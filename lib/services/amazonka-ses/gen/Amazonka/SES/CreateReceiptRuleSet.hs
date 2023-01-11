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
-- Module      : Amazonka.SES.CreateReceiptRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SES.CreateReceiptRuleSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
    ruleSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
createReceiptRuleSet_ruleSetName :: Lens.Lens' CreateReceiptRuleSet Prelude.Text
createReceiptRuleSet_ruleSetName = Lens.lens (\CreateReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@CreateReceiptRuleSet' {} a -> s {ruleSetName = a} :: CreateReceiptRuleSet)

instance Core.AWSRequest CreateReceiptRuleSet where
  type
    AWSResponse CreateReceiptRuleSet =
      CreateReceiptRuleSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateReceiptRuleSetResult"
      ( \s h x ->
          CreateReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReceiptRuleSet where
  hashWithSalt _salt CreateReceiptRuleSet' {..} =
    _salt `Prelude.hashWithSalt` ruleSetName

instance Prelude.NFData CreateReceiptRuleSet where
  rnf CreateReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName

instance Data.ToHeaders CreateReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateReceiptRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReceiptRuleSet where
  toQuery CreateReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Data.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateReceiptRuleSetResponse' smart constructor.
data CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateReceiptRuleSetResponse
newCreateReceiptRuleSetResponse pHttpStatus_ =
  CreateReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createReceiptRuleSetResponse_httpStatus :: Lens.Lens' CreateReceiptRuleSetResponse Prelude.Int
createReceiptRuleSetResponse_httpStatus = Lens.lens (\CreateReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@CreateReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: CreateReceiptRuleSetResponse)

instance Prelude.NFData CreateReceiptRuleSetResponse where
  rnf CreateReceiptRuleSetResponse' {..} =
    Prelude.rnf httpStatus
