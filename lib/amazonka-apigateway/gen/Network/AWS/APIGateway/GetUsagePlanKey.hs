{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan key of a given key identifier.
module Network.AWS.APIGateway.GetUsagePlanKey
  ( -- * Creating a request
    GetUsagePlanKey (..),
    mkGetUsagePlanKey,

    -- ** Request lenses
    gKeyId,
    gUsagePlanId,

    -- * Destructuring the response
    UsagePlanKey (..),
    mkUsagePlanKey,

    -- ** Response lenses
    upkValue,
    upkName,
    upkId,
    upkType,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to get a usage plan key of a given key identifier.
--
-- /See:/ 'mkGetUsagePlanKey' smart constructor.
data GetUsagePlanKey = GetUsagePlanKey'
  { -- | [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
    keyId :: Lude.Text,
    -- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
    usagePlanId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlanKey' with the minimum fields required to make a request.
--
-- * 'keyId' - [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
-- * 'usagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
mkGetUsagePlanKey ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'usagePlanId'
  Lude.Text ->
  GetUsagePlanKey
mkGetUsagePlanKey pKeyId_ pUsagePlanId_ =
  GetUsagePlanKey' {keyId = pKeyId_, usagePlanId = pUsagePlanId_}

-- | [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKeyId :: Lens.Lens' GetUsagePlanKey Lude.Text
gKeyId = Lens.lens (keyId :: GetUsagePlanKey -> Lude.Text) (\s a -> s {keyId = a} :: GetUsagePlanKey)
{-# DEPRECATED gKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gUsagePlanId :: Lens.Lens' GetUsagePlanKey Lude.Text
gUsagePlanId = Lens.lens (usagePlanId :: GetUsagePlanKey -> Lude.Text) (\s a -> s {usagePlanId = a} :: GetUsagePlanKey)
{-# DEPRECATED gUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

instance Lude.AWSRequest GetUsagePlanKey where
  type Rs GetUsagePlanKey = UsagePlanKey
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetUsagePlanKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetUsagePlanKey where
  toPath GetUsagePlanKey' {..} =
    Lude.mconcat
      ["/usageplans/", Lude.toBS usagePlanId, "/keys/", Lude.toBS keyId]

instance Lude.ToQuery GetUsagePlanKey where
  toQuery = Lude.const Lude.mempty
