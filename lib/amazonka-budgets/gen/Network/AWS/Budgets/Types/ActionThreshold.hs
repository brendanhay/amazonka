{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionThreshold where

import Network.AWS.Budgets.Types.ThresholdType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The trigger threshold of the action.
--
--
--
-- /See:/ 'actionThreshold' smart constructor.
data ActionThreshold = ActionThreshold'
  { _atActionThresholdValue ::
      !Double,
    _atActionThresholdType :: !ThresholdType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionThreshold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atActionThresholdValue' - Undocumented member.
--
-- * 'atActionThresholdType' - Undocumented member.
actionThreshold ::
  -- | 'atActionThresholdValue'
  Double ->
  -- | 'atActionThresholdType'
  ThresholdType ->
  ActionThreshold
actionThreshold pActionThresholdValue_ pActionThresholdType_ =
  ActionThreshold'
    { _atActionThresholdValue =
        pActionThresholdValue_,
      _atActionThresholdType = pActionThresholdType_
    }

-- | Undocumented member.
atActionThresholdValue :: Lens' ActionThreshold Double
atActionThresholdValue = lens _atActionThresholdValue (\s a -> s {_atActionThresholdValue = a})

-- | Undocumented member.
atActionThresholdType :: Lens' ActionThreshold ThresholdType
atActionThresholdType = lens _atActionThresholdType (\s a -> s {_atActionThresholdType = a})

instance FromJSON ActionThreshold where
  parseJSON =
    withObject
      "ActionThreshold"
      ( \x ->
          ActionThreshold'
            <$> (x .: "ActionThresholdValue") <*> (x .: "ActionThresholdType")
      )

instance Hashable ActionThreshold

instance NFData ActionThreshold

instance ToJSON ActionThreshold where
  toJSON ActionThreshold' {..} =
    object
      ( catMaybes
          [ Just ("ActionThresholdValue" .= _atActionThresholdValue),
            Just ("ActionThresholdType" .= _atActionThresholdType)
          ]
      )
