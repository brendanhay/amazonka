{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FulfillmentActivity where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.CodeHook
import Network.AWS.LexModels.Types.FulfillmentActivityType
import Network.AWS.Prelude

-- | Describes how the intent is fulfilled after the user provides all of the information required for the intent. You can provide a Lambda function to process the intent, or you can return the intent information to the client application. We recommend that you use a Lambda function so that the relevant logic lives in the Cloud and limit the client-side code primarily to presentation. If you need to update the logic, you only update the Lambda function; you don't need to upgrade your client application.
--
--
-- Consider the following examples:
--
--     * In a pizza ordering application, after the user provides all of the information for placing an order, you use a Lambda function to place an order with a pizzeria.
--
--     * In a gaming application, when a user says "pick up a rock," this information must go back to the client application so that it can perform the operation and update the graphics. In this case, you want Amazon Lex to return the intent data to the client.
--
--
--
--
-- /See:/ 'fulfillmentActivity' smart constructor.
data FulfillmentActivity = FulfillmentActivity'
  { _faCodeHook ::
      !(Maybe CodeHook),
    _faType :: !FulfillmentActivityType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FulfillmentActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCodeHook' - A description of the Lambda function that is run to fulfill the intent.
--
-- * 'faType' - How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
fulfillmentActivity ::
  -- | 'faType'
  FulfillmentActivityType ->
  FulfillmentActivity
fulfillmentActivity pType_ =
  FulfillmentActivity' {_faCodeHook = Nothing, _faType = pType_}

-- | A description of the Lambda function that is run to fulfill the intent.
faCodeHook :: Lens' FulfillmentActivity (Maybe CodeHook)
faCodeHook = lens _faCodeHook (\s a -> s {_faCodeHook = a})

-- | How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
faType :: Lens' FulfillmentActivity FulfillmentActivityType
faType = lens _faType (\s a -> s {_faType = a})

instance FromJSON FulfillmentActivity where
  parseJSON =
    withObject
      "FulfillmentActivity"
      ( \x ->
          FulfillmentActivity' <$> (x .:? "codeHook") <*> (x .: "type")
      )

instance Hashable FulfillmentActivity

instance NFData FulfillmentActivity

instance ToJSON FulfillmentActivity where
  toJSON FulfillmentActivity' {..} =
    object
      ( catMaybes
          [("codeHook" .=) <$> _faCodeHook, Just ("type" .= _faType)]
      )
