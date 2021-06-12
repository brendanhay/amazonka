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

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetRulePriorities' smart constructor.
data SetRulePriorities = SetRulePriorities'
  { -- | The rule priorities.
    rulePriorities :: [RulePriorityPair]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  SetRulePriorities' {rulePriorities = Core.mempty}

-- | The rule priorities.
setRulePriorities_rulePriorities :: Lens.Lens' SetRulePriorities [RulePriorityPair]
setRulePriorities_rulePriorities = Lens.lens (\SetRulePriorities' {rulePriorities} -> rulePriorities) (\s@SetRulePriorities' {} a -> s {rulePriorities = a} :: SetRulePriorities) Core.. Lens._Coerce

instance Core.AWSRequest SetRulePriorities where
  type
    AWSResponse SetRulePriorities =
      SetRulePrioritiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetRulePrioritiesResult"
      ( \s h x ->
          SetRulePrioritiesResponse'
            Core.<$> ( x Core..@? "Rules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetRulePriorities

instance Core.NFData SetRulePriorities

instance Core.ToHeaders SetRulePriorities where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetRulePriorities where
  toPath = Core.const "/"

instance Core.ToQuery SetRulePriorities where
  toQuery SetRulePriorities' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetRulePriorities" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "RulePriorities"
          Core.=: Core.toQueryList "member" rulePriorities
      ]

-- | /See:/ 'newSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { -- | Information about the rules.
    rules :: Core.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SetRulePrioritiesResponse
newSetRulePrioritiesResponse pHttpStatus_ =
  SetRulePrioritiesResponse'
    { rules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the rules.
setRulePrioritiesResponse_rules :: Lens.Lens' SetRulePrioritiesResponse (Core.Maybe [Rule])
setRulePrioritiesResponse_rules = Lens.lens (\SetRulePrioritiesResponse' {rules} -> rules) (\s@SetRulePrioritiesResponse' {} a -> s {rules = a} :: SetRulePrioritiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
setRulePrioritiesResponse_httpStatus :: Lens.Lens' SetRulePrioritiesResponse Core.Int
setRulePrioritiesResponse_httpStatus = Lens.lens (\SetRulePrioritiesResponse' {httpStatus} -> httpStatus) (\s@SetRulePrioritiesResponse' {} a -> s {httpStatus = a} :: SetRulePrioritiesResponse)

instance Core.NFData SetRulePrioritiesResponse
