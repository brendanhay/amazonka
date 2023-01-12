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
-- Module      : Amazonka.IoT.DisableTopicRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the rule.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DisableTopicRule>
-- action.
module Amazonka.IoT.DisableTopicRule
  ( -- * Creating a Request
    DisableTopicRule (..),
    newDisableTopicRule,

    -- * Request Lenses
    disableTopicRule_ruleName,

    -- * Destructuring the Response
    DisableTopicRuleResponse (..),
    newDisableTopicRuleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DisableTopicRuleRequest operation.
--
-- /See:/ 'newDisableTopicRule' smart constructor.
data DisableTopicRule = DisableTopicRule'
  { -- | The name of the rule to disable.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'disableTopicRule_ruleName' - The name of the rule to disable.
newDisableTopicRule ::
  -- | 'ruleName'
  Prelude.Text ->
  DisableTopicRule
newDisableTopicRule pRuleName_ =
  DisableTopicRule' {ruleName = pRuleName_}

-- | The name of the rule to disable.
disableTopicRule_ruleName :: Lens.Lens' DisableTopicRule Prelude.Text
disableTopicRule_ruleName = Lens.lens (\DisableTopicRule' {ruleName} -> ruleName) (\s@DisableTopicRule' {} a -> s {ruleName = a} :: DisableTopicRule)

instance Core.AWSRequest DisableTopicRule where
  type
    AWSResponse DisableTopicRule =
      DisableTopicRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisableTopicRuleResponse'

instance Prelude.Hashable DisableTopicRule where
  hashWithSalt _salt DisableTopicRule' {..} =
    _salt `Prelude.hashWithSalt` ruleName

instance Prelude.NFData DisableTopicRule where
  rnf DisableTopicRule' {..} = Prelude.rnf ruleName

instance Data.ToHeaders DisableTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DisableTopicRule where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisableTopicRule where
  toPath DisableTopicRule' {..} =
    Prelude.mconcat
      ["/rules/", Data.toBS ruleName, "/disable"]

instance Data.ToQuery DisableTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableTopicRuleResponse' smart constructor.
data DisableTopicRuleResponse = DisableTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableTopicRuleResponse ::
  DisableTopicRuleResponse
newDisableTopicRuleResponse =
  DisableTopicRuleResponse'

instance Prelude.NFData DisableTopicRuleResponse where
  rnf _ = ()
