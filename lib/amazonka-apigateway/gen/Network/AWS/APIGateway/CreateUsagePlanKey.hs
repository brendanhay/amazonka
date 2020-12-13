{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan key for adding an existing API key to a usage plan.
module Network.AWS.APIGateway.CreateUsagePlanKey
  ( -- * Creating a request
    CreateUsagePlanKey (..),
    mkCreateUsagePlanKey,

    -- ** Request lenses
    cupkKeyType,
    cupkKeyId,
    cupkUsagePlanId,

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

-- | The POST request to create a usage plan key for adding an existing API key to a usage plan.
--
-- /See:/ 'mkCreateUsagePlanKey' smart constructor.
data CreateUsagePlanKey = CreateUsagePlanKey'
  { -- | [Required] The type of a 'UsagePlanKey' resource for a plan customer.
    keyType :: Lude.Text,
    -- | [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
    keyId :: Lude.Text,
    -- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
    usagePlanId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUsagePlanKey' with the minimum fields required to make a request.
--
-- * 'keyType' - [Required] The type of a 'UsagePlanKey' resource for a plan customer.
-- * 'keyId' - [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
-- * 'usagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
mkCreateUsagePlanKey ::
  -- | 'keyType'
  Lude.Text ->
  -- | 'keyId'
  Lude.Text ->
  -- | 'usagePlanId'
  Lude.Text ->
  CreateUsagePlanKey
mkCreateUsagePlanKey pKeyType_ pKeyId_ pUsagePlanId_ =
  CreateUsagePlanKey'
    { keyType = pKeyType_,
      keyId = pKeyId_,
      usagePlanId = pUsagePlanId_
    }

-- | [Required] The type of a 'UsagePlanKey' resource for a plan customer.
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkKeyType :: Lens.Lens' CreateUsagePlanKey Lude.Text
cupkKeyType = Lens.lens (keyType :: CreateUsagePlanKey -> Lude.Text) (\s a -> s {keyType = a} :: CreateUsagePlanKey)
{-# DEPRECATED cupkKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

-- | [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkKeyId :: Lens.Lens' CreateUsagePlanKey Lude.Text
cupkKeyId = Lens.lens (keyId :: CreateUsagePlanKey -> Lude.Text) (\s a -> s {keyId = a} :: CreateUsagePlanKey)
{-# DEPRECATED cupkKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkUsagePlanId :: Lens.Lens' CreateUsagePlanKey Lude.Text
cupkUsagePlanId = Lens.lens (usagePlanId :: CreateUsagePlanKey -> Lude.Text) (\s a -> s {usagePlanId = a} :: CreateUsagePlanKey)
{-# DEPRECATED cupkUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

instance Lude.AWSRequest CreateUsagePlanKey where
  type Rs CreateUsagePlanKey = UsagePlanKey
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateUsagePlanKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateUsagePlanKey where
  toJSON CreateUsagePlanKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("keyType" Lude..= keyType),
            Lude.Just ("keyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath CreateUsagePlanKey where
  toPath CreateUsagePlanKey' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId, "/keys"]

instance Lude.ToQuery CreateUsagePlanKey where
  toQuery = Lude.const Lude.mempty
