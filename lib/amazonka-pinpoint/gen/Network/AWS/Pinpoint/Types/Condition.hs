{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Condition where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Operator
import Network.AWS.Pinpoint.Types.SimpleCondition
import Network.AWS.Prelude

-- | Specifies the conditions to evaluate for an activity in a journey, and how to evaluate those conditions.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cOperator :: !(Maybe Operator),
    _cConditions :: !(Maybe [SimpleCondition])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cOperator' - Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
--
-- * 'cConditions' - The conditions to evaluate for the activity.
condition ::
  Condition
condition =
  Condition' {_cOperator = Nothing, _cConditions = Nothing}

-- | Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
cOperator :: Lens' Condition (Maybe Operator)
cOperator = lens _cOperator (\s a -> s {_cOperator = a})

-- | The conditions to evaluate for the activity.
cConditions :: Lens' Condition [SimpleCondition]
cConditions = lens _cConditions (\s a -> s {_cConditions = a}) . _Default . _Coerce

instance FromJSON Condition where
  parseJSON =
    withObject
      "Condition"
      ( \x ->
          Condition'
            <$> (x .:? "Operator") <*> (x .:? "Conditions" .!= mempty)
      )

instance Hashable Condition

instance NFData Condition

instance ToJSON Condition where
  toJSON Condition' {..} =
    object
      ( catMaybes
          [ ("Operator" .=) <$> _cOperator,
            ("Conditions" .=) <$> _cConditions
          ]
      )
