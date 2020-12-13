{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants a temporary extension to the remaining quota of a usage plan associated with a specified API key.
module Network.AWS.APIGateway.UpdateUsage
  ( -- * Creating a request
    UpdateUsage (..),
    mkUpdateUsage,

    -- ** Request lenses
    uuKeyId,
    uuUsagePlanId,
    uuPatchOperations,

    -- * Destructuring the response
    Usage (..),
    mkUsage,

    -- ** Response lenses
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The PATCH request to grant a temporary extension to the remaining quota of a usage plan associated with a specified API key.
--
-- /See:/ 'mkUpdateUsage' smart constructor.
data UpdateUsage = UpdateUsage'
  { -- | [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
    keyId :: Lude.Text,
    -- | [Required] The Id of the usage plan associated with the usage data.
    usagePlanId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUsage' with the minimum fields required to make a request.
--
-- * 'keyId' - [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
-- * 'usagePlanId' - [Required] The Id of the usage plan associated with the usage data.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateUsage ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'usagePlanId'
  Lude.Text ->
  UpdateUsage
mkUpdateUsage pKeyId_ pUsagePlanId_ =
  UpdateUsage'
    { keyId = pKeyId_,
      usagePlanId = pUsagePlanId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuKeyId :: Lens.Lens' UpdateUsage Lude.Text
uuKeyId = Lens.lens (keyId :: UpdateUsage -> Lude.Text) (\s a -> s {keyId = a} :: UpdateUsage)
{-# DEPRECATED uuKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | [Required] The Id of the usage plan associated with the usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUsagePlanId :: Lens.Lens' UpdateUsage Lude.Text
uuUsagePlanId = Lens.lens (usagePlanId :: UpdateUsage -> Lude.Text) (\s a -> s {usagePlanId = a} :: UpdateUsage)
{-# DEPRECATED uuUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuPatchOperations :: Lens.Lens' UpdateUsage (Lude.Maybe [PatchOperation])
uuPatchOperations = Lens.lens (patchOperations :: UpdateUsage -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateUsage)
{-# DEPRECATED uuPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateUsage where
  type Rs UpdateUsage = Usage
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateUsage where
  toJSON UpdateUsage' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateUsage where
  toPath UpdateUsage' {..} =
    Lude.mconcat
      [ "/usageplans/",
        Lude.toBS usagePlanId,
        "/keys/",
        Lude.toBS keyId,
        "/usage"
      ]

instance Lude.ToQuery UpdateUsage where
  toQuery = Lude.const Lude.mempty
