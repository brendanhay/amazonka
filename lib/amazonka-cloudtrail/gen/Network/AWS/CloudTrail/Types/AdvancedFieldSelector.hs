{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedFieldSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedFieldSelector where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'advancedFieldSelector' smart constructor.
data AdvancedFieldSelector = AdvancedFieldSelector'
  { _afsEndsWith ::
      !(Maybe (List1 Text)),
    _afsNotStartsWith :: !(Maybe (List1 Text)),
    _afsEquals :: !(Maybe (List1 Text)),
    _afsNotEquals :: !(Maybe (List1 Text)),
    _afsNotEndsWith :: !(Maybe (List1 Text)),
    _afsStartsWith :: !(Maybe (List1 Text)),
    _afsField :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdvancedFieldSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afsEndsWith' - Undocumented member.
--
-- * 'afsNotStartsWith' - Undocumented member.
--
-- * 'afsEquals' - Undocumented member.
--
-- * 'afsNotEquals' - Undocumented member.
--
-- * 'afsNotEndsWith' - Undocumented member.
--
-- * 'afsStartsWith' - Undocumented member.
--
-- * 'afsField' - Undocumented member.
advancedFieldSelector ::
  -- | 'afsField'
  Text ->
  AdvancedFieldSelector
advancedFieldSelector pField_ =
  AdvancedFieldSelector'
    { _afsEndsWith = Nothing,
      _afsNotStartsWith = Nothing,
      _afsEquals = Nothing,
      _afsNotEquals = Nothing,
      _afsNotEndsWith = Nothing,
      _afsStartsWith = Nothing,
      _afsField = pField_
    }

-- | Undocumented member.
afsEndsWith :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsEndsWith = lens _afsEndsWith (\s a -> s {_afsEndsWith = a}) . mapping _List1

-- | Undocumented member.
afsNotStartsWith :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsNotStartsWith = lens _afsNotStartsWith (\s a -> s {_afsNotStartsWith = a}) . mapping _List1

-- | Undocumented member.
afsEquals :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsEquals = lens _afsEquals (\s a -> s {_afsEquals = a}) . mapping _List1

-- | Undocumented member.
afsNotEquals :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsNotEquals = lens _afsNotEquals (\s a -> s {_afsNotEquals = a}) . mapping _List1

-- | Undocumented member.
afsNotEndsWith :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsNotEndsWith = lens _afsNotEndsWith (\s a -> s {_afsNotEndsWith = a}) . mapping _List1

-- | Undocumented member.
afsStartsWith :: Lens' AdvancedFieldSelector (Maybe (NonEmpty Text))
afsStartsWith = lens _afsStartsWith (\s a -> s {_afsStartsWith = a}) . mapping _List1

-- | Undocumented member.
afsField :: Lens' AdvancedFieldSelector Text
afsField = lens _afsField (\s a -> s {_afsField = a})

instance FromJSON AdvancedFieldSelector where
  parseJSON =
    withObject
      "AdvancedFieldSelector"
      ( \x ->
          AdvancedFieldSelector'
            <$> (x .:? "EndsWith")
            <*> (x .:? "NotStartsWith")
            <*> (x .:? "Equals")
            <*> (x .:? "NotEquals")
            <*> (x .:? "NotEndsWith")
            <*> (x .:? "StartsWith")
            <*> (x .: "Field")
      )

instance Hashable AdvancedFieldSelector

instance NFData AdvancedFieldSelector

instance ToJSON AdvancedFieldSelector where
  toJSON AdvancedFieldSelector' {..} =
    object
      ( catMaybes
          [ ("EndsWith" .=) <$> _afsEndsWith,
            ("NotStartsWith" .=) <$> _afsNotStartsWith,
            ("Equals" .=) <$> _afsEquals,
            ("NotEquals" .=) <$> _afsNotEquals,
            ("NotEndsWith" .=) <$> _afsNotEndsWith,
            ("StartsWith" .=) <$> _afsStartsWith,
            Just ("Field" .= _afsField)
          ]
      )
