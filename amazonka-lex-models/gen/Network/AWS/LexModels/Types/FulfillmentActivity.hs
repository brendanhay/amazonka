{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FulfillmentActivity where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.FulfillmentActivityType
import qualified Network.AWS.Prelude as Prelude

-- | Describes how the intent is fulfilled after the user provides all of the
-- information required for the intent. You can provide a Lambda function
-- to process the intent, or you can return the intent information to the
-- client application. We recommend that you use a Lambda function so that
-- the relevant logic lives in the Cloud and limit the client-side code
-- primarily to presentation. If you need to update the logic, you only
-- update the Lambda function; you don\'t need to upgrade your client
-- application.
--
-- Consider the following examples:
--
-- -   In a pizza ordering application, after the user provides all of the
--     information for placing an order, you use a Lambda function to place
--     an order with a pizzeria.
--
-- -   In a gaming application, when a user says \"pick up a rock,\" this
--     information must go back to the client application so that it can
--     perform the operation and update the graphics. In this case, you
--     want Amazon Lex to return the intent data to the client.
--
-- /See:/ 'newFulfillmentActivity' smart constructor.
data FulfillmentActivity = FulfillmentActivity'
  { -- | A description of the Lambda function that is run to fulfill the intent.
    codeHook :: Prelude.Maybe CodeHook,
    -- | How the intent should be fulfilled, either by running a Lambda function
    -- or by returning the slot data to the client application.
    type' :: FulfillmentActivityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FulfillmentActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeHook', 'fulfillmentActivity_codeHook' - A description of the Lambda function that is run to fulfill the intent.
--
-- 'type'', 'fulfillmentActivity_type' - How the intent should be fulfilled, either by running a Lambda function
-- or by returning the slot data to the client application.
newFulfillmentActivity ::
  -- | 'type''
  FulfillmentActivityType ->
  FulfillmentActivity
newFulfillmentActivity pType_ =
  FulfillmentActivity'
    { codeHook = Prelude.Nothing,
      type' = pType_
    }

-- | A description of the Lambda function that is run to fulfill the intent.
fulfillmentActivity_codeHook :: Lens.Lens' FulfillmentActivity (Prelude.Maybe CodeHook)
fulfillmentActivity_codeHook = Lens.lens (\FulfillmentActivity' {codeHook} -> codeHook) (\s@FulfillmentActivity' {} a -> s {codeHook = a} :: FulfillmentActivity)

-- | How the intent should be fulfilled, either by running a Lambda function
-- or by returning the slot data to the client application.
fulfillmentActivity_type :: Lens.Lens' FulfillmentActivity FulfillmentActivityType
fulfillmentActivity_type = Lens.lens (\FulfillmentActivity' {type'} -> type') (\s@FulfillmentActivity' {} a -> s {type' = a} :: FulfillmentActivity)

instance Prelude.FromJSON FulfillmentActivity where
  parseJSON =
    Prelude.withObject
      "FulfillmentActivity"
      ( \x ->
          FulfillmentActivity'
            Prelude.<$> (x Prelude..:? "codeHook")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable FulfillmentActivity

instance Prelude.NFData FulfillmentActivity

instance Prelude.ToJSON FulfillmentActivity where
  toJSON FulfillmentActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("codeHook" Prelude..=) Prelude.<$> codeHook,
            Prelude.Just ("type" Prelude..= type')
          ]
      )
