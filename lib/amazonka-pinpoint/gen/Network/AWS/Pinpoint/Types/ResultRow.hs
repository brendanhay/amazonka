{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ResultRow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ResultRow where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ResultRowValue
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
--
--
-- /See:/ 'resultRow' smart constructor.
data ResultRow = ResultRow'
  { _rrGroupedBys :: ![ResultRowValue],
    _rrValues :: ![ResultRowValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultRow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrGroupedBys' - An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
--
-- * 'rrValues' - An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
resultRow ::
  ResultRow
resultRow = ResultRow' {_rrGroupedBys = mempty, _rrValues = mempty}

-- | An array of objects that defines the field and field values that were used to group data in a result set that contains multiple results. This value is null if the data in a result set isn’t grouped.
rrGroupedBys :: Lens' ResultRow [ResultRowValue]
rrGroupedBys = lens _rrGroupedBys (\s a -> s {_rrGroupedBys = a}) . _Coerce

-- | An array of objects that provides pre-aggregated values for a standard metric that applies to an application, campaign, or journey.
rrValues :: Lens' ResultRow [ResultRowValue]
rrValues = lens _rrValues (\s a -> s {_rrValues = a}) . _Coerce

instance FromJSON ResultRow where
  parseJSON =
    withObject
      "ResultRow"
      ( \x ->
          ResultRow'
            <$> (x .:? "GroupedBys" .!= mempty) <*> (x .:? "Values" .!= mempty)
      )

instance Hashable ResultRow

instance NFData ResultRow
