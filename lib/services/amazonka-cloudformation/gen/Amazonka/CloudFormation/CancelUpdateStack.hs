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
-- Module      : Amazonka.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes
-- successfully, the stack rolls back the update and reverts to the
-- previous stack configuration.
--
-- You can cancel only stacks that are in the @UPDATE_IN_PROGRESS@ state.
module Amazonka.CloudFormation.CancelUpdateStack
  ( -- * Creating a Request
    CancelUpdateStack (..),
    newCancelUpdateStack,

    -- * Request Lenses
    cancelUpdateStack_clientRequestToken,
    cancelUpdateStack_stackName,

    -- * Destructuring the Response
    CancelUpdateStackResponse (..),
    newCancelUpdateStackResponse,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CancelUpdateStack action.
--
-- /See:/ 'newCancelUpdateStack' smart constructor.
data CancelUpdateStack = CancelUpdateStack'
  { -- | A unique identifier for this @CancelUpdateStack@ request. Specify this
    -- token if you plan to retry requests so that CloudFormation knows that
    -- you\'re not attempting to cancel an update on a stack with the same
    -- name. You might retry @CancelUpdateStack@ requests to ensure that
    -- CloudFormation successfully received them.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that\'s associated with the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelUpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'cancelUpdateStack_clientRequestToken' - A unique identifier for this @CancelUpdateStack@ request. Specify this
-- token if you plan to retry requests so that CloudFormation knows that
-- you\'re not attempting to cancel an update on a stack with the same
-- name. You might retry @CancelUpdateStack@ requests to ensure that
-- CloudFormation successfully received them.
--
-- 'stackName', 'cancelUpdateStack_stackName' - The name or the unique stack ID that\'s associated with the stack.
newCancelUpdateStack ::
  -- | 'stackName'
  Prelude.Text ->
  CancelUpdateStack
newCancelUpdateStack pStackName_ =
  CancelUpdateStack'
    { clientRequestToken =
        Prelude.Nothing,
      stackName = pStackName_
    }

-- | A unique identifier for this @CancelUpdateStack@ request. Specify this
-- token if you plan to retry requests so that CloudFormation knows that
-- you\'re not attempting to cancel an update on a stack with the same
-- name. You might retry @CancelUpdateStack@ requests to ensure that
-- CloudFormation successfully received them.
cancelUpdateStack_clientRequestToken :: Lens.Lens' CancelUpdateStack (Prelude.Maybe Prelude.Text)
cancelUpdateStack_clientRequestToken = Lens.lens (\CancelUpdateStack' {clientRequestToken} -> clientRequestToken) (\s@CancelUpdateStack' {} a -> s {clientRequestToken = a} :: CancelUpdateStack)

-- | The name or the unique stack ID that\'s associated with the stack.
cancelUpdateStack_stackName :: Lens.Lens' CancelUpdateStack Prelude.Text
cancelUpdateStack_stackName = Lens.lens (\CancelUpdateStack' {stackName} -> stackName) (\s@CancelUpdateStack' {} a -> s {stackName = a} :: CancelUpdateStack)

instance Core.AWSRequest CancelUpdateStack where
  type
    AWSResponse CancelUpdateStack =
      CancelUpdateStackResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CancelUpdateStackResponse'

instance Prelude.Hashable CancelUpdateStack where
  hashWithSalt _salt CancelUpdateStack' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData CancelUpdateStack where
  rnf CancelUpdateStack' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders CancelUpdateStack where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelUpdateStack where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelUpdateStack where
  toQuery CancelUpdateStack' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelUpdateStack" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ClientRequestToken" Data.=: clientRequestToken,
        "StackName" Data.=: stackName
      ]

-- | /See:/ 'newCancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse = CancelUpdateStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelUpdateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelUpdateStackResponse ::
  CancelUpdateStackResponse
newCancelUpdateStackResponse =
  CancelUpdateStackResponse'

instance Prelude.NFData CancelUpdateStackResponse where
  rnf _ = ()
