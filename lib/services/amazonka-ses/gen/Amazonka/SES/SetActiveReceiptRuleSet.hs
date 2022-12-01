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
-- Module      : Amazonka.SES.SetActiveReceiptRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SES.SetActiveReceiptRuleSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to set a receipt rule set as the active receipt
-- rule set. You use receipt rule sets to receive email with Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetActiveReceiptRuleSet' smart constructor.
data SetActiveReceiptRuleSet = SetActiveReceiptRuleSet'
  { -- | The name of the receipt rule set to make active. Setting this value to
    -- null disables all email receiving.
    ruleSetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The name of the receipt rule set to make active. Setting this value to
-- null disables all email receiving.
setActiveReceiptRuleSet_ruleSetName :: Lens.Lens' SetActiveReceiptRuleSet (Prelude.Maybe Prelude.Text)
setActiveReceiptRuleSet_ruleSetName = Lens.lens (\SetActiveReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@SetActiveReceiptRuleSet' {} a -> s {ruleSetName = a} :: SetActiveReceiptRuleSet)

instance Core.AWSRequest SetActiveReceiptRuleSet where
  type
    AWSResponse SetActiveReceiptRuleSet =
      SetActiveReceiptRuleSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetActiveReceiptRuleSetResult"
      ( \s h x ->
          SetActiveReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetActiveReceiptRuleSet where
  hashWithSalt _salt SetActiveReceiptRuleSet' {..} =
    _salt `Prelude.hashWithSalt` ruleSetName

instance Prelude.NFData SetActiveReceiptRuleSet where
  rnf SetActiveReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName

instance Core.ToHeaders SetActiveReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SetActiveReceiptRuleSet where
  toPath = Prelude.const "/"

instance Core.ToQuery SetActiveReceiptRuleSet where
  toQuery SetActiveReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SetActiveReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetActiveReceiptRuleSetResponse' smart constructor.
data SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SetActiveReceiptRuleSetResponse
newSetActiveReceiptRuleSetResponse pHttpStatus_ =
  SetActiveReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setActiveReceiptRuleSetResponse_httpStatus :: Lens.Lens' SetActiveReceiptRuleSetResponse Prelude.Int
setActiveReceiptRuleSetResponse_httpStatus = Lens.lens (\SetActiveReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@SetActiveReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: SetActiveReceiptRuleSetResponse)

instance
  Prelude.NFData
    SetActiveReceiptRuleSetResponse
  where
  rnf SetActiveReceiptRuleSetResponse' {..} =
    Prelude.rnf httpStatus
