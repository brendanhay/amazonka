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
-- Module      : Amazonka.ELBV2.SetRulePriorities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the priorities of the specified rules.
--
-- You can reorder the rules as long as there are no priority conflicts in
-- the new order. Any existing rules that you do not specify retain their
-- current priority.
module Amazonka.ELBV2.SetRulePriorities
  ( -- * Creating a Request
    SetRulePriorities (..),
    newSetRulePriorities,

    -- * Request Lenses
    setRulePriorities_rulePriorities,

    -- * Destructuring the Response
    SetRulePrioritiesResponse (..),
    newSetRulePrioritiesResponse,

    -- * Response Lenses
    setRulePrioritiesResponse_rules,
    setRulePrioritiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetRulePriorities' smart constructor.
data SetRulePriorities = SetRulePriorities'
  { -- | The rule priorities.
    rulePriorities :: [RulePriorityPair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetRulePriorities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rulePriorities', 'setRulePriorities_rulePriorities' - The rule priorities.
newSetRulePriorities ::
  SetRulePriorities
newSetRulePriorities =
  SetRulePriorities' {rulePriorities = Prelude.mempty}

-- | The rule priorities.
setRulePriorities_rulePriorities :: Lens.Lens' SetRulePriorities [RulePriorityPair]
setRulePriorities_rulePriorities = Lens.lens (\SetRulePriorities' {rulePriorities} -> rulePriorities) (\s@SetRulePriorities' {} a -> s {rulePriorities = a} :: SetRulePriorities) Prelude.. Lens.coerced

instance Core.AWSRequest SetRulePriorities where
  type
    AWSResponse SetRulePriorities =
      SetRulePrioritiesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetRulePrioritiesResult"
      ( \s h x ->
          SetRulePrioritiesResponse'
            Prelude.<$> ( x Data..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetRulePriorities where
  hashWithSalt _salt SetRulePriorities' {..} =
    _salt `Prelude.hashWithSalt` rulePriorities

instance Prelude.NFData SetRulePriorities where
  rnf SetRulePriorities' {..} =
    Prelude.rnf rulePriorities

instance Data.ToHeaders SetRulePriorities where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetRulePriorities where
  toPath = Prelude.const "/"

instance Data.ToQuery SetRulePriorities where
  toQuery SetRulePriorities' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetRulePriorities" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "RulePriorities"
          Data.=: Data.toQueryList "member" rulePriorities
      ]

-- | /See:/ 'newSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { -- | Information about the rules.
    rules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetRulePrioritiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'setRulePrioritiesResponse_rules' - Information about the rules.
--
-- 'httpStatus', 'setRulePrioritiesResponse_httpStatus' - The response's http status code.
newSetRulePrioritiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetRulePrioritiesResponse
newSetRulePrioritiesResponse pHttpStatus_ =
  SetRulePrioritiesResponse'
    { rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the rules.
setRulePrioritiesResponse_rules :: Lens.Lens' SetRulePrioritiesResponse (Prelude.Maybe [Rule])
setRulePrioritiesResponse_rules = Lens.lens (\SetRulePrioritiesResponse' {rules} -> rules) (\s@SetRulePrioritiesResponse' {} a -> s {rules = a} :: SetRulePrioritiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
setRulePrioritiesResponse_httpStatus :: Lens.Lens' SetRulePrioritiesResponse Prelude.Int
setRulePrioritiesResponse_httpStatus = Lens.lens (\SetRulePrioritiesResponse' {httpStatus} -> httpStatus) (\s@SetRulePrioritiesResponse' {} a -> s {httpStatus = a} :: SetRulePrioritiesResponse)

instance Prelude.NFData SetRulePrioritiesResponse where
  rnf SetRulePrioritiesResponse' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
