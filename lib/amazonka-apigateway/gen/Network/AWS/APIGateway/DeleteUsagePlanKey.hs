{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan key and remove the underlying API key from the associated usage plan.
module Network.AWS.APIGateway.DeleteUsagePlanKey
  ( -- * Creating a request
    DeleteUsagePlanKey (..),
    mkDeleteUsagePlanKey,

    -- ** Request lenses
    dupkKeyId,
    dupkUsagePlanId,

    -- * Destructuring the response
    DeleteUsagePlanKeyResponse (..),
    mkDeleteUsagePlanKeyResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The DELETE request to delete a usage plan key and remove the underlying API key from the associated usage plan.
--
-- /See:/ 'mkDeleteUsagePlanKey' smart constructor.
data DeleteUsagePlanKey = DeleteUsagePlanKey'
  { -- | [Required] The Id of the 'UsagePlanKey' resource to be deleted.
    keyId :: Lude.Text,
    -- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
    usagePlanId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUsagePlanKey' with the minimum fields required to make a request.
--
-- * 'keyId' - [Required] The Id of the 'UsagePlanKey' resource to be deleted.
-- * 'usagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
mkDeleteUsagePlanKey ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'usagePlanId'
  Lude.Text ->
  DeleteUsagePlanKey
mkDeleteUsagePlanKey pKeyId_ pUsagePlanId_ =
  DeleteUsagePlanKey' {keyId = pKeyId_, usagePlanId = pUsagePlanId_}

-- | [Required] The Id of the 'UsagePlanKey' resource to be deleted.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupkKeyId :: Lens.Lens' DeleteUsagePlanKey Lude.Text
dupkKeyId = Lens.lens (keyId :: DeleteUsagePlanKey -> Lude.Text) (\s a -> s {keyId = a} :: DeleteUsagePlanKey)
{-# DEPRECATED dupkKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupkUsagePlanId :: Lens.Lens' DeleteUsagePlanKey Lude.Text
dupkUsagePlanId = Lens.lens (usagePlanId :: DeleteUsagePlanKey -> Lude.Text) (\s a -> s {usagePlanId = a} :: DeleteUsagePlanKey)
{-# DEPRECATED dupkUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

instance Lude.AWSRequest DeleteUsagePlanKey where
  type Rs DeleteUsagePlanKey = DeleteUsagePlanKeyResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteUsagePlanKeyResponse'

instance Lude.ToHeaders DeleteUsagePlanKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteUsagePlanKey where
  toPath DeleteUsagePlanKey' {..} =
    Lude.mconcat
      ["/usageplans/", Lude.toBS usagePlanId, "/keys/", Lude.toBS keyId]

instance Lude.ToQuery DeleteUsagePlanKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUsagePlanKeyResponse' smart constructor.
data DeleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUsagePlanKeyResponse' with the minimum fields required to make a request.
mkDeleteUsagePlanKeyResponse ::
  DeleteUsagePlanKeyResponse
mkDeleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'
