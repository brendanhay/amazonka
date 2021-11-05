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
-- Module      : Network.AWS.Detective.AcceptInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an invitation for the member account to contribute data to a
-- behavior graph. This operation can only be called by an invited member
-- account.
--
-- The request provides the ARN of behavior graph.
--
-- The member account status in the graph must be @INVITED@.
module Network.AWS.Detective.AcceptInvitation
  ( -- * Creating a Request
    AcceptInvitation (..),
    newAcceptInvitation,

    -- * Request Lenses
    acceptInvitation_graphArn,

    -- * Destructuring the Response
    AcceptInvitationResponse (..),
    newAcceptInvitationResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Detective.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The ARN of the behavior graph that the member account is accepting the
    -- invitation for.
    --
    -- The member account status in the behavior graph must be @INVITED@.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'acceptInvitation_graphArn' - The ARN of the behavior graph that the member account is accepting the
-- invitation for.
--
-- The member account status in the behavior graph must be @INVITED@.
newAcceptInvitation ::
  -- | 'graphArn'
  Prelude.Text ->
  AcceptInvitation
newAcceptInvitation pGraphArn_ =
  AcceptInvitation' {graphArn = pGraphArn_}

-- | The ARN of the behavior graph that the member account is accepting the
-- invitation for.
--
-- The member account status in the behavior graph must be @INVITED@.
acceptInvitation_graphArn :: Lens.Lens' AcceptInvitation Prelude.Text
acceptInvitation_graphArn = Lens.lens (\AcceptInvitation' {graphArn} -> graphArn) (\s@AcceptInvitation' {} a -> s {graphArn = a} :: AcceptInvitation)

instance Core.AWSRequest AcceptInvitation where
  type
    AWSResponse AcceptInvitation =
      AcceptInvitationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull AcceptInvitationResponse'

instance Prelude.Hashable AcceptInvitation

instance Prelude.NFData AcceptInvitation

instance Core.ToHeaders AcceptInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcceptInvitation where
  toJSON AcceptInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArn" Core..= graphArn)]
      )

instance Core.ToPath AcceptInvitation where
  toPath = Prelude.const "/invitation"

instance Core.ToQuery AcceptInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptInvitationResponse' smart constructor.
data AcceptInvitationResponse = AcceptInvitationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAcceptInvitationResponse ::
  AcceptInvitationResponse
newAcceptInvitationResponse =
  AcceptInvitationResponse'

instance Prelude.NFData AcceptInvitationResponse
