{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyRuleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyRuleAction where

import Network.AWS.ECR.Types.ImageActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type of action to be taken.
--
--
--
-- /See:/ 'lifecyclePolicyRuleAction' smart constructor.
newtype LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { _lpraType ::
      Maybe ImageActionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecyclePolicyRuleAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpraType' - The type of action to be taken.
lifecyclePolicyRuleAction ::
  LifecyclePolicyRuleAction
lifecyclePolicyRuleAction =
  LifecyclePolicyRuleAction' {_lpraType = Nothing}

-- | The type of action to be taken.
lpraType :: Lens' LifecyclePolicyRuleAction (Maybe ImageActionType)
lpraType = lens _lpraType (\s a -> s {_lpraType = a})

instance FromJSON LifecyclePolicyRuleAction where
  parseJSON =
    withObject
      "LifecyclePolicyRuleAction"
      (\x -> LifecyclePolicyRuleAction' <$> (x .:? "type"))

instance Hashable LifecyclePolicyRuleAction

instance NFData LifecyclePolicyRuleAction
