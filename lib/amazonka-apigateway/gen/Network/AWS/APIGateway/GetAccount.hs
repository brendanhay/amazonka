{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'Account' resource.
module Network.AWS.APIGateway.GetAccount
  ( -- * Creating a request
    GetAccount (..),
    mkGetAccount,

    -- * Destructuring the response
    Account (..),
    mkAccount,

    -- ** Response lenses
    aApiKeyVersion,
    aCloudwatchRoleARN,
    aFeatures,
    aThrottleSettings,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to get information about the current 'Account' resource.
--
-- /See:/ 'mkGetAccount' smart constructor.
data GetAccount = GetAccount'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccount' with the minimum fields required to make a request.
mkGetAccount ::
  GetAccount
mkGetAccount = GetAccount'

instance Lude.AWSRequest GetAccount where
  type Rs GetAccount = Account
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetAccount where
  toPath = Lude.const "/account"

instance Lude.ToQuery GetAccount where
  toQuery = Lude.const Lude.mempty
