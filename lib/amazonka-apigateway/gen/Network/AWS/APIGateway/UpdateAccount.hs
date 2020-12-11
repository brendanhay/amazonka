{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the current 'Account' resource.
module Network.AWS.APIGateway.UpdateAccount
  ( -- * Creating a request
    UpdateAccount (..),
    mkUpdateAccount,

    -- ** Request lenses
    uaPatchOperations,

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

-- | Requests API Gateway to change information about the current 'Account' resource.
--
-- /See:/ 'mkUpdateAccount' smart constructor.
newtype UpdateAccount = UpdateAccount'
  { patchOperations ::
      Lude.Maybe [PatchOperation]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccount' with the minimum fields required to make a request.
--
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateAccount ::
  UpdateAccount
mkUpdateAccount = UpdateAccount' {patchOperations = Lude.Nothing}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaPatchOperations :: Lens.Lens' UpdateAccount (Lude.Maybe [PatchOperation])
uaPatchOperations = Lens.lens (patchOperations :: UpdateAccount -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateAccount)
{-# DEPRECATED uaPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateAccount where
  type Rs UpdateAccount = Account
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateAccount where
  toJSON UpdateAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateAccount where
  toPath = Lude.const "/account"

instance Lude.ToQuery UpdateAccount where
  toQuery = Lude.const Lude.mempty
