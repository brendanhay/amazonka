{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Usage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Usage
  ( Usage (..),

    -- * Smart constructor
    mkUsage,

    -- * Lenses
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the usage data of a usage plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> , <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan>
--
-- /See:/ 'mkUsage' smart constructor.
data Usage = Usage'
  { usagePlanId :: Lude.Maybe Lude.Text,
    endDate :: Lude.Maybe Lude.Text,
    items :: Lude.Maybe (Lude.HashMap Lude.Text ([[Lude.Integer]])),
    startDate :: Lude.Maybe Lude.Text,
    position :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Usage' with the minimum fields required to make a request.
--
-- * 'endDate' - The ending date of the usage data.
-- * 'items' - The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
-- * 'position' - Undocumented field.
-- * 'startDate' - The starting date of the usage data.
-- * 'usagePlanId' - The plan Id associated with this usage data.
mkUsage ::
  Usage
mkUsage =
  Usage'
    { usagePlanId = Lude.Nothing,
      endDate = Lude.Nothing,
      items = Lude.Nothing,
      startDate = Lude.Nothing,
      position = Lude.Nothing
    }

-- | The plan Id associated with this usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsagePlanId :: Lens.Lens' Usage (Lude.Maybe Lude.Text)
uUsagePlanId = Lens.lens (usagePlanId :: Usage -> Lude.Maybe Lude.Text) (\s a -> s {usagePlanId = a} :: Usage)
{-# DEPRECATED uUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

-- | The ending date of the usage data.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEndDate :: Lens.Lens' Usage (Lude.Maybe Lude.Text)
uEndDate = Lens.lens (endDate :: Usage -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: Usage)
{-# DEPRECATED uEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uItems :: Lens.Lens' Usage (Lude.Maybe (Lude.HashMap Lude.Text ([[Lude.Integer]])))
uItems = Lens.lens (items :: Usage -> Lude.Maybe (Lude.HashMap Lude.Text ([[Lude.Integer]]))) (\s a -> s {items = a} :: Usage)
{-# DEPRECATED uItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The starting date of the usage data.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStartDate :: Lens.Lens' Usage (Lude.Maybe Lude.Text)
uStartDate = Lens.lens (startDate :: Usage -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: Usage)
{-# DEPRECATED uStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPosition :: Lens.Lens' Usage (Lude.Maybe Lude.Text)
uPosition = Lens.lens (position :: Usage -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: Usage)
{-# DEPRECATED uPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Lude.FromJSON Usage where
  parseJSON =
    Lude.withObject
      "Usage"
      ( \x ->
          Usage'
            Lude.<$> (x Lude..:? "usagePlanId")
            Lude.<*> (x Lude..:? "endDate")
            Lude.<*> (x Lude..:? "values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startDate")
            Lude.<*> (x Lude..:? "position")
      )
