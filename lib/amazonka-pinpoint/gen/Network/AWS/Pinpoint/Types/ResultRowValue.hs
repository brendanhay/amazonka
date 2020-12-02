{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ResultRowValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ResultRowValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a single value and metadata about that value as part of an array of query results for a standard metric that applies to an application, campaign, or journey.
--
--
--
-- /See:/ 'resultRowValue' smart constructor.
data ResultRowValue = ResultRowValue'
  { _rrvType :: !Text,
    _rrvValue :: !Text,
    _rrvKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultRowValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrvType' - The data type of the value specified by the Value property.
--
-- * 'rrvValue' - In a Values object, the value for the metric that the query retrieved data for. In a GroupedBys object, the value for the field that was used to group data in a result set that contains multiple results (Values objects).
--
-- * 'rrvKey' - The friendly name of the metric whose value is specified by the Value property.
resultRowValue ::
  -- | 'rrvType'
  Text ->
  -- | 'rrvValue'
  Text ->
  -- | 'rrvKey'
  Text ->
  ResultRowValue
resultRowValue pType_ pValue_ pKey_ =
  ResultRowValue'
    { _rrvType = pType_,
      _rrvValue = pValue_,
      _rrvKey = pKey_
    }

-- | The data type of the value specified by the Value property.
rrvType :: Lens' ResultRowValue Text
rrvType = lens _rrvType (\s a -> s {_rrvType = a})

-- | In a Values object, the value for the metric that the query retrieved data for. In a GroupedBys object, the value for the field that was used to group data in a result set that contains multiple results (Values objects).
rrvValue :: Lens' ResultRowValue Text
rrvValue = lens _rrvValue (\s a -> s {_rrvValue = a})

-- | The friendly name of the metric whose value is specified by the Value property.
rrvKey :: Lens' ResultRowValue Text
rrvKey = lens _rrvKey (\s a -> s {_rrvKey = a})

instance FromJSON ResultRowValue where
  parseJSON =
    withObject
      "ResultRowValue"
      ( \x ->
          ResultRowValue'
            <$> (x .: "Type") <*> (x .: "Value") <*> (x .: "Key")
      )

instance Hashable ResultRowValue

instance NFData ResultRowValue
