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
-- Module      : Amazonka.SES.CloneReceiptRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule set by cloning an existing one. All receipt rules
-- and configurations are copied to the new receipt rule set and are
-- completely independent of the source rule set.
--
-- For information about setting up rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.CloneReceiptRuleSet
  ( -- * Creating a Request
    CloneReceiptRuleSet (..),
    newCloneReceiptRuleSet,

    -- * Request Lenses
    cloneReceiptRuleSet_ruleSetName,
    cloneReceiptRuleSet_originalRuleSetName,

    -- * Destructuring the Response
    CloneReceiptRuleSetResponse (..),
    newCloneReceiptRuleSetResponse,

    -- * Response Lenses
    cloneReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to create a receipt rule set by cloning an existing
-- one. You use receipt rule sets to receive email with Amazon SES. For
-- more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCloneReceiptRuleSet' smart constructor.
data CloneReceiptRuleSet = CloneReceiptRuleSet'
  { -- | The name of the rule set to create. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Start and end with a letter or number.
    --
    -- -   Contain less than 64 characters.
    ruleSetName :: Prelude.Text,
    -- | The name of the rule set to clone.
    originalRuleSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'cloneReceiptRuleSet_ruleSetName' - The name of the rule set to create. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
--
-- 'originalRuleSetName', 'cloneReceiptRuleSet_originalRuleSetName' - The name of the rule set to clone.
newCloneReceiptRuleSet ::
  -- | 'ruleSetName'
  Prelude.Text ->
  -- | 'originalRuleSetName'
  Prelude.Text ->
  CloneReceiptRuleSet
newCloneReceiptRuleSet
  pRuleSetName_
  pOriginalRuleSetName_ =
    CloneReceiptRuleSet'
      { ruleSetName = pRuleSetName_,
        originalRuleSetName = pOriginalRuleSetName_
      }

-- | The name of the rule set to create. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
cloneReceiptRuleSet_ruleSetName :: Lens.Lens' CloneReceiptRuleSet Prelude.Text
cloneReceiptRuleSet_ruleSetName = Lens.lens (\CloneReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@CloneReceiptRuleSet' {} a -> s {ruleSetName = a} :: CloneReceiptRuleSet)

-- | The name of the rule set to clone.
cloneReceiptRuleSet_originalRuleSetName :: Lens.Lens' CloneReceiptRuleSet Prelude.Text
cloneReceiptRuleSet_originalRuleSetName = Lens.lens (\CloneReceiptRuleSet' {originalRuleSetName} -> originalRuleSetName) (\s@CloneReceiptRuleSet' {} a -> s {originalRuleSetName = a} :: CloneReceiptRuleSet)

instance Core.AWSRequest CloneReceiptRuleSet where
  type
    AWSResponse CloneReceiptRuleSet =
      CloneReceiptRuleSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CloneReceiptRuleSetResult"
      ( \s h x ->
          CloneReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CloneReceiptRuleSet where
  hashWithSalt _salt CloneReceiptRuleSet' {..} =
    _salt
      `Prelude.hashWithSalt` ruleSetName
      `Prelude.hashWithSalt` originalRuleSetName

instance Prelude.NFData CloneReceiptRuleSet where
  rnf CloneReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName
      `Prelude.seq` Prelude.rnf originalRuleSetName

instance Data.ToHeaders CloneReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CloneReceiptRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CloneReceiptRuleSet where
  toQuery CloneReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CloneReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Data.=: ruleSetName,
        "OriginalRuleSetName" Data.=: originalRuleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCloneReceiptRuleSetResponse' smart constructor.
data CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cloneReceiptRuleSetResponse_httpStatus' - The response's http status code.
newCloneReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CloneReceiptRuleSetResponse
newCloneReceiptRuleSetResponse pHttpStatus_ =
  CloneReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cloneReceiptRuleSetResponse_httpStatus :: Lens.Lens' CloneReceiptRuleSetResponse Prelude.Int
cloneReceiptRuleSetResponse_httpStatus = Lens.lens (\CloneReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@CloneReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: CloneReceiptRuleSetResponse)

instance Prelude.NFData CloneReceiptRuleSetResponse where
  rnf CloneReceiptRuleSetResponse' {..} =
    Prelude.rnf httpStatus
