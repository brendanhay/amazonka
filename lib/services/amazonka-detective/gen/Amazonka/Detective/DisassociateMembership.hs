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
-- Module      : Amazonka.Detective.DisassociateMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the member account from the specified behavior graph. This
-- operation can only be called by an invited member account that has the
-- @ENABLED@ status.
--
-- @DisassociateMembership@ cannot be called by an organization account in
-- the organization behavior graph. For the organization behavior graph,
-- the Detective administrator account determines which organization
-- accounts to enable or disable as member accounts.
module Amazonka.Detective.DisassociateMembership
  ( -- * Creating a Request
    DisassociateMembership (..),
    newDisassociateMembership,

    -- * Request Lenses
    disassociateMembership_graphArn,

    -- * Destructuring the Response
    DisassociateMembershipResponse (..),
    newDisassociateMembershipResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMembership' smart constructor.
data DisassociateMembership = DisassociateMembership'
  { -- | The ARN of the behavior graph to remove the member account from.
    --
    -- The member account\'s member status in the behavior graph must be
    -- @ENABLED@.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'disassociateMembership_graphArn' - The ARN of the behavior graph to remove the member account from.
--
-- The member account\'s member status in the behavior graph must be
-- @ENABLED@.
newDisassociateMembership ::
  -- | 'graphArn'
  Prelude.Text ->
  DisassociateMembership
newDisassociateMembership pGraphArn_ =
  DisassociateMembership' {graphArn = pGraphArn_}

-- | The ARN of the behavior graph to remove the member account from.
--
-- The member account\'s member status in the behavior graph must be
-- @ENABLED@.
disassociateMembership_graphArn :: Lens.Lens' DisassociateMembership Prelude.Text
disassociateMembership_graphArn = Lens.lens (\DisassociateMembership' {graphArn} -> graphArn) (\s@DisassociateMembership' {} a -> s {graphArn = a} :: DisassociateMembership)

instance Core.AWSRequest DisassociateMembership where
  type
    AWSResponse DisassociateMembership =
      DisassociateMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateMembershipResponse'

instance Prelude.Hashable DisassociateMembership where
  hashWithSalt _salt DisassociateMembership' {..} =
    _salt `Prelude.hashWithSalt` graphArn

instance Prelude.NFData DisassociateMembership where
  rnf DisassociateMembership' {..} =
    Prelude.rnf graphArn

instance Data.ToHeaders DisassociateMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateMembership where
  toJSON DisassociateMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArn" Data..= graphArn)]
      )

instance Data.ToPath DisassociateMembership where
  toPath = Prelude.const "/membership/removal"

instance Data.ToQuery DisassociateMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMembershipResponse' smart constructor.
data DisassociateMembershipResponse = DisassociateMembershipResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateMembershipResponse ::
  DisassociateMembershipResponse
newDisassociateMembershipResponse =
  DisassociateMembershipResponse'

instance
  Prelude.NFData
    DisassociateMembershipResponse
  where
  rnf _ = ()
