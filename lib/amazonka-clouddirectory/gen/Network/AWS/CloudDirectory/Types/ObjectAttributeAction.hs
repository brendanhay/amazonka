{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeAction where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The action to take on the object attribute.
--
--
--
-- /See:/ 'objectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
  { _oaaObjectAttributeActionType ::
      !(Maybe UpdateActionType),
    _oaaObjectAttributeUpdateValue ::
      !(Maybe TypedAttributeValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectAttributeAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaaObjectAttributeActionType' - A type that can be either @Update@ or @Delete@ .
--
-- * 'oaaObjectAttributeUpdateValue' - The value that you want to update to.
objectAttributeAction ::
  ObjectAttributeAction
objectAttributeAction =
  ObjectAttributeAction'
    { _oaaObjectAttributeActionType = Nothing,
      _oaaObjectAttributeUpdateValue = Nothing
    }

-- | A type that can be either @Update@ or @Delete@ .
oaaObjectAttributeActionType :: Lens' ObjectAttributeAction (Maybe UpdateActionType)
oaaObjectAttributeActionType = lens _oaaObjectAttributeActionType (\s a -> s {_oaaObjectAttributeActionType = a})

-- | The value that you want to update to.
oaaObjectAttributeUpdateValue :: Lens' ObjectAttributeAction (Maybe TypedAttributeValue)
oaaObjectAttributeUpdateValue = lens _oaaObjectAttributeUpdateValue (\s a -> s {_oaaObjectAttributeUpdateValue = a})

instance Hashable ObjectAttributeAction

instance NFData ObjectAttributeAction

instance ToJSON ObjectAttributeAction where
  toJSON ObjectAttributeAction' {..} =
    object
      ( catMaybes
          [ ("ObjectAttributeActionType" .=)
              <$> _oaaObjectAttributeActionType,
            ("ObjectAttributeUpdateValue" .=)
              <$> _oaaObjectAttributeUpdateValue
          ]
      )
