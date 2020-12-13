{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan of a given plan identifier.
module Network.AWS.APIGateway.GetUsagePlan
  ( -- * Creating a request
    GetUsagePlan (..),
    mkGetUsagePlan,

    -- ** Request lenses
    gupUsagePlanId,

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

-- | The GET request to get a usage plan of a given plan identifier.
--
-- /See:/ 'mkGetUsagePlan' smart constructor.
newtype GetUsagePlan = GetUsagePlan'
  { -- | [Required] The identifier of the 'UsagePlan' resource to be retrieved.
    usagePlanId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlan' with the minimum fields required to make a request.
--
-- * 'usagePlanId' - [Required] The identifier of the 'UsagePlan' resource to be retrieved.
mkGetUsagePlan ::
  -- | 'usagePlanId'
  Lude.Text ->
  GetUsagePlan
mkGetUsagePlan pUsagePlanId_ =
  GetUsagePlan' {usagePlanId = pUsagePlanId_}

-- | [Required] The identifier of the 'UsagePlan' resource to be retrieved.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupUsagePlanId :: Lens.Lens' GetUsagePlan Lude.Text
gupUsagePlanId = Lens.lens (usagePlanId :: GetUsagePlan -> Lude.Text) (\s a -> s {usagePlanId = a} :: GetUsagePlan)
{-# DEPRECATED gupUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

instance Lude.AWSRequest GetUsagePlan where
  type Rs GetUsagePlan = UsagePlan
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetUsagePlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetUsagePlan where
  toPath GetUsagePlan' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId]

instance Lude.ToQuery GetUsagePlan where
  toQuery = Lude.const Lude.mempty
