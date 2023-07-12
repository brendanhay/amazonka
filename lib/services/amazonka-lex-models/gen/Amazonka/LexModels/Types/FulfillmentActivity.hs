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
-- Module      : Amazonka.LexModels.Types.FulfillmentActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.FulfillmentActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.CodeHook
import Amazonka.LexModels.Types.FulfillmentActivityType
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON FulfillmentActivity where
  parseJSON =
    Data.withObject
      "FulfillmentActivity"
      ( \x ->
          FulfillmentActivity'
            Prelude.<$> (x Data..:? "codeHook")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FulfillmentActivity where
  hashWithSalt _salt FulfillmentActivity' {..} =
    _salt
      `Prelude.hashWithSalt` codeHook
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FulfillmentActivity where
  rnf FulfillmentActivity' {..} =
    Prelude.rnf codeHook
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FulfillmentActivity where
  toJSON FulfillmentActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("codeHook" Data..=) Prelude.<$> codeHook,
            Prelude.Just ("type" Data..= type')
          ]
      )
