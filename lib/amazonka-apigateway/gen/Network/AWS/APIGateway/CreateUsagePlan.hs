{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the associated API stages, specified in the payload.
module Network.AWS.APIGateway.CreateUsagePlan
  ( -- * Creating a request
    CreateUsagePlan (..),
    mkCreateUsagePlan,

    -- ** Request lenses
    cupApiStages,
    cupThrottle,
    cupQuota,
    cupDescription,
    cupTags,
    cupName,

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

-- | The POST request to create a usage plan with the name, description, throttle limits and quota limits, as well as the associated API stages, specified in the payload.
--
-- /See:/ 'mkCreateUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { apiStages ::
      Lude.Maybe [APIStage],
    throttle :: Lude.Maybe ThrottleSettings,
    quota :: Lude.Maybe QuotaSettings,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUsagePlan' with the minimum fields required to make a request.
--
-- * 'apiStages' - The associated API stages of the usage plan.
-- * 'description' - The description of the usage plan.
-- * 'name' - [Required] The name of the usage plan.
-- * 'quota' - The quota of the usage plan.
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
-- * 'throttle' - The throttling limits of the usage plan.
mkCreateUsagePlan ::
  -- | 'name'
  Lude.Text ->
  CreateUsagePlan
mkCreateUsagePlan pName_ =
  CreateUsagePlan'
    { apiStages = Lude.Nothing,
      throttle = Lude.Nothing,
      quota = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | The associated API stages of the usage plan.
--
-- /Note:/ Consider using 'apiStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupApiStages :: Lens.Lens' CreateUsagePlan (Lude.Maybe [APIStage])
cupApiStages = Lens.lens (apiStages :: CreateUsagePlan -> Lude.Maybe [APIStage]) (\s a -> s {apiStages = a} :: CreateUsagePlan)
{-# DEPRECATED cupApiStages "Use generic-lens or generic-optics with 'apiStages' instead." #-}

-- | The throttling limits of the usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupThrottle :: Lens.Lens' CreateUsagePlan (Lude.Maybe ThrottleSettings)
cupThrottle = Lens.lens (throttle :: CreateUsagePlan -> Lude.Maybe ThrottleSettings) (\s a -> s {throttle = a} :: CreateUsagePlan)
{-# DEPRECATED cupThrottle "Use generic-lens or generic-optics with 'throttle' instead." #-}

-- | The quota of the usage plan.
--
-- /Note:/ Consider using 'quota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupQuota :: Lens.Lens' CreateUsagePlan (Lude.Maybe QuotaSettings)
cupQuota = Lens.lens (quota :: CreateUsagePlan -> Lude.Maybe QuotaSettings) (\s a -> s {quota = a} :: CreateUsagePlan)
{-# DEPRECATED cupQuota "Use generic-lens or generic-optics with 'quota' instead." #-}

-- | The description of the usage plan.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDescription :: Lens.Lens' CreateUsagePlan (Lude.Maybe Lude.Text)
cupDescription = Lens.lens (description :: CreateUsagePlan -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateUsagePlan)
{-# DEPRECATED cupDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupTags :: Lens.Lens' CreateUsagePlan (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cupTags = Lens.lens (tags :: CreateUsagePlan -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateUsagePlan)
{-# DEPRECATED cupTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | [Required] The name of the usage plan.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupName :: Lens.Lens' CreateUsagePlan Lude.Text
cupName = Lens.lens (name :: CreateUsagePlan -> Lude.Text) (\s a -> s {name = a} :: CreateUsagePlan)
{-# DEPRECATED cupName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateUsagePlan where
  type Rs CreateUsagePlan = UsagePlan
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateUsagePlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateUsagePlan where
  toJSON CreateUsagePlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("apiStages" Lude..=) Lude.<$> apiStages,
            ("throttle" Lude..=) Lude.<$> throttle,
            ("quota" Lude..=) Lude.<$> quota,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateUsagePlan where
  toPath = Lude.const "/usageplans"

instance Lude.ToQuery CreateUsagePlan where
  toQuery = Lude.const Lude.mempty
