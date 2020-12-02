{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.AddAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.AddAttributesActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that adds other attributes based on existing attributes in the message.
--
--
--
-- /See:/ 'addAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { _aaaNext ::
      !(Maybe Text),
    _aaaName :: !Text,
    _aaaAttributes :: !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaaNext' - The next activity in the pipeline.
--
-- * 'aaaName' - The name of the addAttributes activity.
--
-- * 'aaaAttributes' - A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
addAttributesActivity ::
  -- | 'aaaName'
  Text ->
  AddAttributesActivity
addAttributesActivity pName_ =
  AddAttributesActivity'
    { _aaaNext = Nothing,
      _aaaName = pName_,
      _aaaAttributes = mempty
    }

-- | The next activity in the pipeline.
aaaNext :: Lens' AddAttributesActivity (Maybe Text)
aaaNext = lens _aaaNext (\s a -> s {_aaaNext = a})

-- | The name of the addAttributes activity.
aaaName :: Lens' AddAttributesActivity Text
aaaName = lens _aaaName (\s a -> s {_aaaName = a})

-- | A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
aaaAttributes :: Lens' AddAttributesActivity (HashMap Text (Text))
aaaAttributes = lens _aaaAttributes (\s a -> s {_aaaAttributes = a}) . _Map

instance FromJSON AddAttributesActivity where
  parseJSON =
    withObject
      "AddAttributesActivity"
      ( \x ->
          AddAttributesActivity'
            <$> (x .:? "next")
            <*> (x .: "name")
            <*> (x .:? "attributes" .!= mempty)
      )

instance Hashable AddAttributesActivity

instance NFData AddAttributesActivity

instance ToJSON AddAttributesActivity where
  toJSON AddAttributesActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _aaaNext,
            Just ("name" .= _aaaName),
            Just ("attributes" .= _aaaAttributes)
          ]
      )
