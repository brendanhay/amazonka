{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UsagePlanKey
  ( UsagePlanKey (..),

    -- * Smart constructor
    mkUsagePlanKey,

    -- * Lenses
    upkValue,
    upkName,
    upkId,
    upkType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a usage plan key to identify a plan customer.
--
-- To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected 'ApiKey' .
-- " <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'mkUsagePlanKey' smart constructor.
data UsagePlanKey = UsagePlanKey'
  { -- | The value of a usage plan key.
    value :: Lude.Maybe Lude.Text,
    -- | The name of a usage plan key.
    name :: Lude.Maybe Lude.Text,
    -- | The Id of a usage plan key.
    id :: Lude.Maybe Lude.Text,
    -- | The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsagePlanKey' with the minimum fields required to make a request.
--
-- * 'value' - The value of a usage plan key.
-- * 'name' - The name of a usage plan key.
-- * 'id' - The Id of a usage plan key.
-- * 'type'' - The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
mkUsagePlanKey ::
  UsagePlanKey
mkUsagePlanKey =
  UsagePlanKey'
    { value = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The value of a usage plan key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkValue :: Lens.Lens' UsagePlanKey (Lude.Maybe Lude.Text)
upkValue = Lens.lens (value :: UsagePlanKey -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: UsagePlanKey)
{-# DEPRECATED upkValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of a usage plan key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkName :: Lens.Lens' UsagePlanKey (Lude.Maybe Lude.Text)
upkName = Lens.lens (name :: UsagePlanKey -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UsagePlanKey)
{-# DEPRECATED upkName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Id of a usage plan key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkId :: Lens.Lens' UsagePlanKey (Lude.Maybe Lude.Text)
upkId = Lens.lens (id :: UsagePlanKey -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UsagePlanKey)
{-# DEPRECATED upkId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkType :: Lens.Lens' UsagePlanKey (Lude.Maybe Lude.Text)
upkType = Lens.lens (type' :: UsagePlanKey -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: UsagePlanKey)
{-# DEPRECATED upkType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON UsagePlanKey where
  parseJSON =
    Lude.withObject
      "UsagePlanKey"
      ( \x ->
          UsagePlanKey'
            Lude.<$> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "type")
      )
