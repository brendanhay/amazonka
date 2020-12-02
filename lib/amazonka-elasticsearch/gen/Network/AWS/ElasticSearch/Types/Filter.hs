{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter used to limit results when describing inbound or outbound cross-cluster search connections. Multiple values can be specified per filter. A cross-cluster search connection must match at least one of the specified values for it to be returned from an operation.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fValues :: !(Maybe (List1 Text)),
    _fName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - Contains one or more values for the filter.
--
-- * 'fName' - Specifies the name of the filter.
filter' ::
  Filter
filter' = Filter' {_fValues = Nothing, _fName = Nothing}

-- | Contains one or more values for the filter.
fValues :: Lens' Filter (Maybe (NonEmpty Text))
fValues = lens _fValues (\s a -> s {_fValues = a}) . mapping _List1

-- | Specifies the name of the filter.
fName :: Lens' Filter (Maybe Text)
fName = lens _fName (\s a -> s {_fName = a})

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      (catMaybes [("Values" .=) <$> _fValues, ("Name" .=) <$> _fName])
