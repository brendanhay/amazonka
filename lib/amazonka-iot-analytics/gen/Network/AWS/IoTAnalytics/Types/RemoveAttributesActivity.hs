{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that removes attributes from a message.
--
--
--
-- /See:/ 'removeAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { _raaNext ::
      !(Maybe Text),
    _raaName :: !Text,
    _raaAttributes :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raaNext' - The next activity in the pipeline.
--
-- * 'raaName' - The name of the @removeAttributes@ activity.
--
-- * 'raaAttributes' - A list of 1-50 attributes to remove from the message.
removeAttributesActivity ::
  -- | 'raaName'
  Text ->
  -- | 'raaAttributes'
  NonEmpty Text ->
  RemoveAttributesActivity
removeAttributesActivity pName_ pAttributes_ =
  RemoveAttributesActivity'
    { _raaNext = Nothing,
      _raaName = pName_,
      _raaAttributes = _List1 # pAttributes_
    }

-- | The next activity in the pipeline.
raaNext :: Lens' RemoveAttributesActivity (Maybe Text)
raaNext = lens _raaNext (\s a -> s {_raaNext = a})

-- | The name of the @removeAttributes@ activity.
raaName :: Lens' RemoveAttributesActivity Text
raaName = lens _raaName (\s a -> s {_raaName = a})

-- | A list of 1-50 attributes to remove from the message.
raaAttributes :: Lens' RemoveAttributesActivity (NonEmpty Text)
raaAttributes = lens _raaAttributes (\s a -> s {_raaAttributes = a}) . _List1

instance FromJSON RemoveAttributesActivity where
  parseJSON =
    withObject
      "RemoveAttributesActivity"
      ( \x ->
          RemoveAttributesActivity'
            <$> (x .:? "next") <*> (x .: "name") <*> (x .: "attributes")
      )

instance Hashable RemoveAttributesActivity

instance NFData RemoveAttributesActivity

instance ToJSON RemoveAttributesActivity where
  toJSON RemoveAttributesActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _raaNext,
            Just ("name" .= _raaName),
            Just ("attributes" .= _raaAttributes)
          ]
      )
