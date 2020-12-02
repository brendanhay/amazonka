{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter that can use conditional operators.
--
--
-- For more information about filters, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html Querying Discovered Configuration Items> in the /AWS Application Discovery Service User Guide/ .
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fName :: !Text,
    _fValues :: ![Text],
    _fCondition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter.
--
-- * 'fValues' - A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
--
-- * 'fCondition' - A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
filter' ::
  -- | 'fName'
  Text ->
  -- | 'fCondition'
  Text ->
  Filter
filter' pName_ pCondition_ =
  Filter'
    { _fName = pName_,
      _fValues = mempty,
      _fCondition = pCondition_
    }

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

-- | A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Coerce

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
fCondition :: Lens' Filter Text
fCondition = lens _fCondition (\s a -> s {_fCondition = a})

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _fName),
            Just ("values" .= _fValues),
            Just ("condition" .= _fCondition)
          ]
      )
