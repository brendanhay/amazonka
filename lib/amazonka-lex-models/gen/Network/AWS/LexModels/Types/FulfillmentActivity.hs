{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FulfillmentActivity
  ( FulfillmentActivity (..),

    -- * Smart constructor
    mkFulfillmentActivity,

    -- * Lenses
    faCodeHook,
    faType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.FulfillmentActivityType
import qualified Network.AWS.Prelude as Lude

-- | Describes how the intent is fulfilled after the user provides all of the information required for the intent. You can provide a Lambda function to process the intent, or you can return the intent information to the client application. We recommend that you use a Lambda function so that the relevant logic lives in the Cloud and limit the client-side code primarily to presentation. If you need to update the logic, you only update the Lambda function; you don't need to upgrade your client application.
--
-- Consider the following examples:
--
--     * In a pizza ordering application, after the user provides all of the information for placing an order, you use a Lambda function to place an order with a pizzeria.
--
--
--     * In a gaming application, when a user says "pick up a rock," this information must go back to the client application so that it can perform the operation and update the graphics. In this case, you want Amazon Lex to return the intent data to the client.
--
--
--
-- /See:/ 'mkFulfillmentActivity' smart constructor.
data FulfillmentActivity = FulfillmentActivity'
  { -- | A description of the Lambda function that is run to fulfill the intent.
    codeHook :: Lude.Maybe CodeHook,
    -- | How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
    type' :: FulfillmentActivityType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FulfillmentActivity' with the minimum fields required to make a request.
--
-- * 'codeHook' - A description of the Lambda function that is run to fulfill the intent.
-- * 'type'' - How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
mkFulfillmentActivity ::
  -- | 'type''
  FulfillmentActivityType ->
  FulfillmentActivity
mkFulfillmentActivity pType_ =
  FulfillmentActivity' {codeHook = Lude.Nothing, type' = pType_}

-- | A description of the Lambda function that is run to fulfill the intent.
--
-- /Note:/ Consider using 'codeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCodeHook :: Lens.Lens' FulfillmentActivity (Lude.Maybe CodeHook)
faCodeHook = Lens.lens (codeHook :: FulfillmentActivity -> Lude.Maybe CodeHook) (\s a -> s {codeHook = a} :: FulfillmentActivity)
{-# DEPRECATED faCodeHook "Use generic-lens or generic-optics with 'codeHook' instead." #-}

-- | How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faType :: Lens.Lens' FulfillmentActivity FulfillmentActivityType
faType = Lens.lens (type' :: FulfillmentActivity -> FulfillmentActivityType) (\s a -> s {type' = a} :: FulfillmentActivity)
{-# DEPRECATED faType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FulfillmentActivity where
  parseJSON =
    Lude.withObject
      "FulfillmentActivity"
      ( \x ->
          FulfillmentActivity'
            Lude.<$> (x Lude..:? "codeHook") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON FulfillmentActivity where
  toJSON FulfillmentActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("codeHook" Lude..=) Lude.<$> codeHook,
            Lude.Just ("type" Lude..= type')
          ]
      )
