{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UsagePlan
  ( UsagePlan (..),

    -- * Smart constructor
    mkUsagePlan,

    -- * Lenses
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

import Network.AWS.APIGateway.Types.APIStage
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.ThrottleSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.
--
-- In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'mkUsagePlan' smart constructor.
data UsagePlan = UsagePlan'
  { apiStages :: Lude.Maybe [APIStage],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    throttle :: Lude.Maybe ThrottleSettings,
    quota :: Lude.Maybe QuotaSettings,
    description :: Lude.Maybe Lude.Text,
    productCode :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsagePlan' with the minimum fields required to make a request.
--
-- * 'apiStages' - The associated API stages of a usage plan.
-- * 'description' - The description of a usage plan.
-- * 'id' - The identifier of a 'UsagePlan' resource.
-- * 'name' - The name of a usage plan.
-- * 'productCode' - The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
-- * 'quota' - The maximum number of permitted requests per a given unit time interval.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
-- * 'throttle' - The request throttle limits of a usage plan.
mkUsagePlan ::
  UsagePlan
mkUsagePlan =
  UsagePlan'
    { apiStages = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      throttle = Lude.Nothing,
      quota = Lude.Nothing,
      description = Lude.Nothing,
      productCode = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The associated API stages of a usage plan.
--
-- /Note:/ Consider using 'apiStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upApiStages :: Lens.Lens' UsagePlan (Lude.Maybe [APIStage])
upApiStages = Lens.lens (apiStages :: UsagePlan -> Lude.Maybe [APIStage]) (\s a -> s {apiStages = a} :: UsagePlan)
{-# DEPRECATED upApiStages "Use generic-lens or generic-optics with 'apiStages' instead." #-}

-- | The name of a usage plan.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UsagePlan (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UsagePlan -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UsagePlan)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of a 'UsagePlan' resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UsagePlan (Lude.Maybe Lude.Text)
upId = Lens.lens (id :: UsagePlan -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UsagePlan)
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The request throttle limits of a usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upThrottle :: Lens.Lens' UsagePlan (Lude.Maybe ThrottleSettings)
upThrottle = Lens.lens (throttle :: UsagePlan -> Lude.Maybe ThrottleSettings) (\s a -> s {throttle = a} :: UsagePlan)
{-# DEPRECATED upThrottle "Use generic-lens or generic-optics with 'throttle' instead." #-}

-- | The maximum number of permitted requests per a given unit time interval.
--
-- /Note:/ Consider using 'quota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upQuota :: Lens.Lens' UsagePlan (Lude.Maybe QuotaSettings)
upQuota = Lens.lens (quota :: UsagePlan -> Lude.Maybe QuotaSettings) (\s a -> s {quota = a} :: UsagePlan)
{-# DEPRECATED upQuota "Use generic-lens or generic-optics with 'quota' instead." #-}

-- | The description of a usage plan.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UsagePlan (Lude.Maybe Lude.Text)
upDescription = Lens.lens (description :: UsagePlan -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UsagePlan)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProductCode :: Lens.Lens' UsagePlan (Lude.Maybe Lude.Text)
upProductCode = Lens.lens (productCode :: UsagePlan -> Lude.Maybe Lude.Text) (\s a -> s {productCode = a} :: UsagePlan)
{-# DEPRECATED upProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTags :: Lens.Lens' UsagePlan (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
upTags = Lens.lens (tags :: UsagePlan -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: UsagePlan)
{-# DEPRECATED upTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON UsagePlan where
  parseJSON =
    Lude.withObject
      "UsagePlan"
      ( \x ->
          UsagePlan'
            Lude.<$> (x Lude..:? "apiStages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "throttle")
            Lude.<*> (x Lude..:? "quota")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "productCode")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
