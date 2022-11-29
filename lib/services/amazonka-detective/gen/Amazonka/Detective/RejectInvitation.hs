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
-- Module      : Amazonka.Detective.RejectInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an invitation to contribute the account data to a behavior
-- graph. This operation must be called by an invited member account that
-- has the @INVITED@ status.
--
-- @RejectInvitation@ cannot be called by an organization account in the
-- organization behavior graph. In the organization behavior graph,
-- organization accounts do not receive an invitation.
module Amazonka.Detective.RejectInvitation
  ( -- * Creating a Request
    RejectInvitation (..),
    newRejectInvitation,

    -- * Request Lenses
    rejectInvitation_graphArn,

    -- * Destructuring the Response
    RejectInvitationResponse (..),
    newRejectInvitationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectInvitation' smart constructor.
data RejectInvitation = RejectInvitation'
  { -- | The ARN of the behavior graph to reject the invitation to.
    --
    -- The member account\'s current member status in the behavior graph must
    -- be @INVITED@.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'rejectInvitation_graphArn' - The ARN of the behavior graph to reject the invitation to.
--
-- The member account\'s current member status in the behavior graph must
-- be @INVITED@.
newRejectInvitation ::
  -- | 'graphArn'
  Prelude.Text ->
  RejectInvitation
newRejectInvitation pGraphArn_ =
  RejectInvitation' {graphArn = pGraphArn_}

-- | The ARN of the behavior graph to reject the invitation to.
--
-- The member account\'s current member status in the behavior graph must
-- be @INVITED@.
rejectInvitation_graphArn :: Lens.Lens' RejectInvitation Prelude.Text
rejectInvitation_graphArn = Lens.lens (\RejectInvitation' {graphArn} -> graphArn) (\s@RejectInvitation' {} a -> s {graphArn = a} :: RejectInvitation)

instance Core.AWSRequest RejectInvitation where
  type
    AWSResponse RejectInvitation =
      RejectInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RejectInvitationResponse'

instance Prelude.Hashable RejectInvitation where
  hashWithSalt _salt RejectInvitation' {..} =
    _salt `Prelude.hashWithSalt` graphArn

instance Prelude.NFData RejectInvitation where
  rnf RejectInvitation' {..} = Prelude.rnf graphArn

instance Core.ToHeaders RejectInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RejectInvitation where
  toJSON RejectInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArn" Core..= graphArn)]
      )

instance Core.ToPath RejectInvitation where
  toPath = Prelude.const "/invitation/removal"

instance Core.ToQuery RejectInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectInvitationResponse' smart constructor.
data RejectInvitationResponse = RejectInvitationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRejectInvitationResponse ::
  RejectInvitationResponse
newRejectInvitationResponse =
  RejectInvitationResponse'

instance Prelude.NFData RejectInvitationResponse where
  rnf _ = ()
