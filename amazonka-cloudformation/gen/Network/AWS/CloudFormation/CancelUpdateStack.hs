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
-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes
-- successfully, the stack rolls back the update and reverts to the
-- previous stack configuration.
--
-- You can cancel only stacks that are in the UPDATE_IN_PROGRESS state.
module Network.AWS.CloudFormation.CancelUpdateStack
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

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CancelUpdateStack action.
--
-- /See:/ 'newCancelUpdateStack' smart constructor.
data CancelUpdateStack = CancelUpdateStack'
  { -- | A unique identifier for this @CancelUpdateStack@ request. Specify this
    -- token if you plan to retry requests so that AWS CloudFormation knows
    -- that you\'re not attempting to cancel an update on a stack with the same
    -- name. You might retry @CancelUpdateStack@ requests to ensure that AWS
    -- CloudFormation successfully received them.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that is associated with the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelUpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'cancelUpdateStack_clientRequestToken' - A unique identifier for this @CancelUpdateStack@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to cancel an update on a stack with the same
-- name. You might retry @CancelUpdateStack@ requests to ensure that AWS
-- CloudFormation successfully received them.
--
-- 'stackName', 'cancelUpdateStack_stackName' - The name or the unique stack ID that is associated with the stack.
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
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to cancel an update on a stack with the same
-- name. You might retry @CancelUpdateStack@ requests to ensure that AWS
-- CloudFormation successfully received them.
cancelUpdateStack_clientRequestToken :: Lens.Lens' CancelUpdateStack (Prelude.Maybe Prelude.Text)
cancelUpdateStack_clientRequestToken = Lens.lens (\CancelUpdateStack' {clientRequestToken} -> clientRequestToken) (\s@CancelUpdateStack' {} a -> s {clientRequestToken = a} :: CancelUpdateStack)

-- | The name or the unique stack ID that is associated with the stack.
cancelUpdateStack_stackName :: Lens.Lens' CancelUpdateStack Prelude.Text
cancelUpdateStack_stackName = Lens.lens (\CancelUpdateStack' {stackName} -> stackName) (\s@CancelUpdateStack' {} a -> s {stackName = a} :: CancelUpdateStack)

instance Prelude.AWSRequest CancelUpdateStack where
  type Rs CancelUpdateStack = CancelUpdateStackResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull CancelUpdateStackResponse'

instance Prelude.Hashable CancelUpdateStack

instance Prelude.NFData CancelUpdateStack

instance Prelude.ToHeaders CancelUpdateStack where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelUpdateStack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelUpdateStack where
  toQuery CancelUpdateStack' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelUpdateStack" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "StackName" Prelude.=: stackName
      ]

-- | /See:/ 'newCancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse = CancelUpdateStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelUpdateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelUpdateStackResponse ::
  CancelUpdateStackResponse
newCancelUpdateStackResponse =
  CancelUpdateStackResponse'

instance Prelude.NFData CancelUpdateStackResponse
