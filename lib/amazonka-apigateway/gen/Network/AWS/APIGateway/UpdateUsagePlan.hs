{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a usage plan of a given plan Id.
module Network.AWS.APIGateway.UpdateUsagePlan
  ( -- * Creating a request
    UpdateUsagePlan (..),
    mkUpdateUsagePlan,

    -- ** Request lenses
    uupUsagePlanId,
    uupPatchOperations,

    -- * Destructuring the response
    UsagePlan (..),
    mkUsagePlan,

    -- ** Response lenses
    upApiStages,
    upName,
    upId,
    upThrottle,
    upQuota,
    upDescription,
    upProductCode,
    upTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The PATCH request to update a usage plan of a given plan Id.
--
-- /See:/ 'mkUpdateUsagePlan' smart constructor.
data UpdateUsagePlan = UpdateUsagePlan'
  { -- | [Required] The Id of the to-be-updated usage plan.
    usagePlanId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUsagePlan' with the minimum fields required to make a request.
--
-- * 'usagePlanId' - [Required] The Id of the to-be-updated usage plan.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateUsagePlan ::
  -- | 'usagePlanId'
  Lude.Text ->
  UpdateUsagePlan
mkUpdateUsagePlan pUsagePlanId_ =
  UpdateUsagePlan'
    { usagePlanId = pUsagePlanId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The Id of the to-be-updated usage plan.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUsagePlanId :: Lens.Lens' UpdateUsagePlan Lude.Text
uupUsagePlanId = Lens.lens (usagePlanId :: UpdateUsagePlan -> Lude.Text) (\s a -> s {usagePlanId = a} :: UpdateUsagePlan)
{-# DEPRECATED uupUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupPatchOperations :: Lens.Lens' UpdateUsagePlan (Lude.Maybe [PatchOperation])
uupPatchOperations = Lens.lens (patchOperations :: UpdateUsagePlan -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateUsagePlan)
{-# DEPRECATED uupPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateUsagePlan where
  type Rs UpdateUsagePlan = UsagePlan
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateUsagePlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateUsagePlan where
  toJSON UpdateUsagePlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateUsagePlan where
  toPath UpdateUsagePlan' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId]

instance Lude.ToQuery UpdateUsagePlan where
  toQuery = Lude.const Lude.mempty
