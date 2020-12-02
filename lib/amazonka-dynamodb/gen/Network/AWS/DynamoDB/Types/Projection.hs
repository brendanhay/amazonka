{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Projection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Projection where

import Network.AWS.DynamoDB.Types.ProjectionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
--
--
-- /See:/ 'projection' smart constructor.
data Projection = Projection'
  { _pProjectionType ::
      !(Maybe ProjectionType),
    _pNonKeyAttributes :: !(Maybe (List1 Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Projection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pProjectionType' - The set of attributes that are projected into the index:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.     * @ALL@ - All of the table attributes are projected into the index.
--
-- * 'pNonKeyAttributes' - Represents the non-key attribute names which will be projected into the index. For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
projection ::
  Projection
projection =
  Projection'
    { _pProjectionType = Nothing,
      _pNonKeyAttributes = Nothing
    }

-- | The set of attributes that are projected into the index:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.     * @ALL@ - All of the table attributes are projected into the index.
pProjectionType :: Lens' Projection (Maybe ProjectionType)
pProjectionType = lens _pProjectionType (\s a -> s {_pProjectionType = a})

-- | Represents the non-key attribute names which will be projected into the index. For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
pNonKeyAttributes :: Lens' Projection (Maybe (NonEmpty Text))
pNonKeyAttributes = lens _pNonKeyAttributes (\s a -> s {_pNonKeyAttributes = a}) . mapping _List1

instance FromJSON Projection where
  parseJSON =
    withObject
      "Projection"
      ( \x ->
          Projection'
            <$> (x .:? "ProjectionType") <*> (x .:? "NonKeyAttributes")
      )

instance Hashable Projection

instance NFData Projection

instance ToJSON Projection where
  toJSON Projection' {..} =
    object
      ( catMaybes
          [ ("ProjectionType" .=) <$> _pProjectionType,
            ("NonKeyAttributes" .=) <$> _pNonKeyAttributes
          ]
      )
