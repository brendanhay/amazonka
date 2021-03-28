{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.FulfillmentActivity
  ( FulfillmentActivity (..)
  -- * Smart constructor
  , mkFulfillmentActivity
  -- * Lenses
  , faType
  , faCodeHook
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.CodeHook as Types
import qualified Network.AWS.LexModels.Types.FulfillmentActivityType as Types
import qualified Network.AWS.Prelude as Core

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
  { type' :: Types.FulfillmentActivityType
    -- ^ How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application. 
  , codeHook :: Core.Maybe Types.CodeHook
    -- ^ A description of the Lambda function that is run to fulfill the intent. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FulfillmentActivity' value with any optional fields omitted.
mkFulfillmentActivity
    :: Types.FulfillmentActivityType -- ^ 'type\''
    -> FulfillmentActivity
mkFulfillmentActivity type'
  = FulfillmentActivity'{type', codeHook = Core.Nothing}

-- | How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application. 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faType :: Lens.Lens' FulfillmentActivity Types.FulfillmentActivityType
faType = Lens.field @"type'"
{-# INLINEABLE faType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A description of the Lambda function that is run to fulfill the intent. 
--
-- /Note:/ Consider using 'codeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCodeHook :: Lens.Lens' FulfillmentActivity (Core.Maybe Types.CodeHook)
faCodeHook = Lens.field @"codeHook"
{-# INLINEABLE faCodeHook #-}
{-# DEPRECATED codeHook "Use generic-lens or generic-optics with 'codeHook' instead"  #-}

instance Core.FromJSON FulfillmentActivity where
        toJSON FulfillmentActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("codeHook" Core..=) Core.<$> codeHook])

instance Core.FromJSON FulfillmentActivity where
        parseJSON
          = Core.withObject "FulfillmentActivity" Core.$
              \ x ->
                FulfillmentActivity' Core.<$>
                  (x Core..: "type") Core.<*> x Core..:? "codeHook"
