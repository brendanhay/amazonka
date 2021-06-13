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
-- Module      : Network.AWS.APIGateway.GetAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current Account resource.
module Network.AWS.APIGateway.GetAccount
  ( -- * Creating a Request
    GetAccount (..),
    newGetAccount,

    -- * Destructuring the Response
    Account (..),
    newAccount,

    -- * Response Lenses
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about the current Account
-- resource.
--
-- /See:/ 'newGetAccount' smart constructor.
data GetAccount = GetAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccount ::
  GetAccount
newGetAccount = GetAccount'

instance Core.AWSRequest GetAccount where
  type AWSResponse GetAccount = Account
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetAccount

instance Prelude.NFData GetAccount

instance Core.ToHeaders GetAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetAccount where
  toPath = Prelude.const "/account"

instance Core.ToQuery GetAccount where
  toQuery = Prelude.const Prelude.mempty
