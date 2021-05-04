{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELBv2.SetRulePriorities
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ELBv2.SetRulePriorities
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

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetRulePriorities' smart constructor.
data SetRulePriorities = SetRulePriorities'
  { -- | The rule priorities.
    rulePriorities :: [RulePriorityPair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
setRulePriorities_rulePriorities = Lens.lens (\SetRulePriorities' {rulePriorities} -> rulePriorities) (\s@SetRulePriorities' {} a -> s {rulePriorities = a} :: SetRulePriorities) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest SetRulePriorities where
  type Rs SetRulePriorities = SetRulePrioritiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetRulePrioritiesResult"
      ( \s h x ->
          SetRulePrioritiesResponse'
            Prelude.<$> ( x Prelude..@? "Rules" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetRulePriorities

instance Prelude.NFData SetRulePriorities

instance Prelude.ToHeaders SetRulePriorities where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetRulePriorities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetRulePriorities where
  toQuery SetRulePriorities' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetRulePriorities" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "RulePriorities"
          Prelude.=: Prelude.toQueryList "member" rulePriorities
      ]

-- | /See:/ 'newSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { -- | Information about the rules.
    rules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
setRulePrioritiesResponse_rules = Lens.lens (\SetRulePrioritiesResponse' {rules} -> rules) (\s@SetRulePrioritiesResponse' {} a -> s {rules = a} :: SetRulePrioritiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
setRulePrioritiesResponse_httpStatus :: Lens.Lens' SetRulePrioritiesResponse Prelude.Int
setRulePrioritiesResponse_httpStatus = Lens.lens (\SetRulePrioritiesResponse' {httpStatus} -> httpStatus) (\s@SetRulePrioritiesResponse' {} a -> s {httpStatus = a} :: SetRulePrioritiesResponse)

instance Prelude.NFData SetRulePrioritiesResponse
