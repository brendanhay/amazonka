{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PropertyPredicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PropertyPredicate where

import Network.AWS.Glue.Types.Comparator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a property predicate.
--
--
--
-- /See:/ 'propertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { _ppValue ::
      !(Maybe Text),
    _ppKey :: !(Maybe Text),
    _ppComparator :: !(Maybe Comparator)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PropertyPredicate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppValue' - The value of the property.
--
-- * 'ppKey' - The key of the property.
--
-- * 'ppComparator' - The comparator used to compare this property to others.
propertyPredicate ::
  PropertyPredicate
propertyPredicate =
  PropertyPredicate'
    { _ppValue = Nothing,
      _ppKey = Nothing,
      _ppComparator = Nothing
    }

-- | The value of the property.
ppValue :: Lens' PropertyPredicate (Maybe Text)
ppValue = lens _ppValue (\s a -> s {_ppValue = a})

-- | The key of the property.
ppKey :: Lens' PropertyPredicate (Maybe Text)
ppKey = lens _ppKey (\s a -> s {_ppKey = a})

-- | The comparator used to compare this property to others.
ppComparator :: Lens' PropertyPredicate (Maybe Comparator)
ppComparator = lens _ppComparator (\s a -> s {_ppComparator = a})

instance Hashable PropertyPredicate

instance NFData PropertyPredicate

instance ToJSON PropertyPredicate where
  toJSON PropertyPredicate' {..} =
    object
      ( catMaybes
          [ ("Value" .=) <$> _ppValue,
            ("Key" .=) <$> _ppKey,
            ("Comparator" .=) <$> _ppComparator
          ]
      )
