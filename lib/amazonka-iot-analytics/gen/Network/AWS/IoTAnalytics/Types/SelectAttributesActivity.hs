{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SelectAttributesActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates a new message using only the specified attributes from the original message.
--
--
--
-- /See:/ 'selectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
  { _saaNext ::
      !(Maybe Text),
    _saaName :: !Text,
    _saaAttributes :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saaNext' - The next activity in the pipeline.
--
-- * 'saaName' - The name of the @selectAttributes@ activity.
--
-- * 'saaAttributes' - A list of the attributes to select from the message.
selectAttributesActivity ::
  -- | 'saaName'
  Text ->
  -- | 'saaAttributes'
  NonEmpty Text ->
  SelectAttributesActivity
selectAttributesActivity pName_ pAttributes_ =
  SelectAttributesActivity'
    { _saaNext = Nothing,
      _saaName = pName_,
      _saaAttributes = _List1 # pAttributes_
    }

-- | The next activity in the pipeline.
saaNext :: Lens' SelectAttributesActivity (Maybe Text)
saaNext = lens _saaNext (\s a -> s {_saaNext = a})

-- | The name of the @selectAttributes@ activity.
saaName :: Lens' SelectAttributesActivity Text
saaName = lens _saaName (\s a -> s {_saaName = a})

-- | A list of the attributes to select from the message.
saaAttributes :: Lens' SelectAttributesActivity (NonEmpty Text)
saaAttributes = lens _saaAttributes (\s a -> s {_saaAttributes = a}) . _List1

instance FromJSON SelectAttributesActivity where
  parseJSON =
    withObject
      "SelectAttributesActivity"
      ( \x ->
          SelectAttributesActivity'
            <$> (x .:? "next") <*> (x .: "name") <*> (x .: "attributes")
      )

instance Hashable SelectAttributesActivity

instance NFData SelectAttributesActivity

instance ToJSON SelectAttributesActivity where
  toJSON SelectAttributesActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _saaNext,
            Just ("name" .= _saaName),
            Just ("attributes" .= _saaAttributes)
          ]
      )
