{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SearchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SearchFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes criteria to restrict a list of results.
--
--
-- For operators that apply a single value to the attribute, the filter is evaluated as follows: @Attribute Operator Values[1]@
--
-- Some operators, e.g. @in@ , can apply multiple values. In this case, the filter is evaluated as a logical union (OR) of applications of the operator to the attribute with each one of the values: @(Attribute Operator Values[1]) OR (Attribute Operator Values[2]) OR ...@
--
-- The valid values for attributes of @SearchFilter@ depend on the API action. For valid values, see the reference page for the API action you're calling that takes a @SearchFilter@ parameter.
--
--
-- /See:/ 'searchFilter' smart constructor.
data SearchFilter = SearchFilter'
  { _sfAttribute :: !(Maybe Text),
    _sfValues :: !(Maybe [Text]),
    _sfOperator :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfAttribute' - The result attribute to which the filter values are applied. Valid values vary by API action.
--
-- * 'sfValues' - The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
--
-- * 'sfOperator' - The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
searchFilter ::
  SearchFilter
searchFilter =
  SearchFilter'
    { _sfAttribute = Nothing,
      _sfValues = Nothing,
      _sfOperator = Nothing
    }

-- | The result attribute to which the filter values are applied. Valid values vary by API action.
sfAttribute :: Lens' SearchFilter (Maybe Text)
sfAttribute = lens _sfAttribute (\s a -> s {_sfAttribute = a})

-- | The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
sfValues :: Lens' SearchFilter [Text]
sfValues = lens _sfValues (\s a -> s {_sfValues = a}) . _Default . _Coerce

-- | The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
sfOperator :: Lens' SearchFilter (Maybe Text)
sfOperator = lens _sfOperator (\s a -> s {_sfOperator = a})

instance Hashable SearchFilter

instance NFData SearchFilter

instance ToQuery SearchFilter where
  toQuery SearchFilter' {..} =
    mconcat
      [ "Attribute" =: _sfAttribute,
        "Values" =: toQuery (toQueryList "member" <$> _sfValues),
        "Operator" =: _sfOperator
      ]
