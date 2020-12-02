{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedEventSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedEventSelector where

import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'advancedEventSelector' smart constructor.
data AdvancedEventSelector = AdvancedEventSelector'
  { _aesName ::
      !Text,
    _aesFieldSelectors ::
      !(List1 AdvancedFieldSelector)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdvancedEventSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aesName' - Undocumented member.
--
-- * 'aesFieldSelectors' - Undocumented member.
advancedEventSelector ::
  -- | 'aesName'
  Text ->
  -- | 'aesFieldSelectors'
  NonEmpty AdvancedFieldSelector ->
  AdvancedEventSelector
advancedEventSelector pName_ pFieldSelectors_ =
  AdvancedEventSelector'
    { _aesName = pName_,
      _aesFieldSelectors = _List1 # pFieldSelectors_
    }

-- | Undocumented member.
aesName :: Lens' AdvancedEventSelector Text
aesName = lens _aesName (\s a -> s {_aesName = a})

-- | Undocumented member.
aesFieldSelectors :: Lens' AdvancedEventSelector (NonEmpty AdvancedFieldSelector)
aesFieldSelectors = lens _aesFieldSelectors (\s a -> s {_aesFieldSelectors = a}) . _List1

instance FromJSON AdvancedEventSelector where
  parseJSON =
    withObject
      "AdvancedEventSelector"
      ( \x ->
          AdvancedEventSelector'
            <$> (x .: "Name") <*> (x .: "FieldSelectors")
      )

instance Hashable AdvancedEventSelector

instance NFData AdvancedEventSelector

instance ToJSON AdvancedEventSelector where
  toJSON AdvancedEventSelector' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _aesName),
            Just ("FieldSelectors" .= _aesFieldSelectors)
          ]
      )
