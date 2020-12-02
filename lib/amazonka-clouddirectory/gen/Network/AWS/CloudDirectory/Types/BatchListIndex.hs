{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndex where

import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists objects attached to the specified index inside a 'BatchRead' operation. For more information, see 'ListIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { _batRangesOnIndexedValues ::
      !(Maybe [ObjectAttributeRange]),
    _batNextToken :: !(Maybe Text),
    _batMaxResults :: !(Maybe Nat),
    _batIndexReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batRangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
--
-- * 'batNextToken' - The pagination token.
--
-- * 'batMaxResults' - The maximum number of results to retrieve.
--
-- * 'batIndexReference' - The reference to the index to list.
batchListIndex ::
  -- | 'batIndexReference'
  ObjectReference ->
  BatchListIndex
batchListIndex pIndexReference_ =
  BatchListIndex'
    { _batRangesOnIndexedValues = Nothing,
      _batNextToken = Nothing,
      _batMaxResults = Nothing,
      _batIndexReference = pIndexReference_
    }

-- | Specifies the ranges of indexed values that you want to query.
batRangesOnIndexedValues :: Lens' BatchListIndex [ObjectAttributeRange]
batRangesOnIndexedValues = lens _batRangesOnIndexedValues (\s a -> s {_batRangesOnIndexedValues = a}) . _Default . _Coerce

-- | The pagination token.
batNextToken :: Lens' BatchListIndex (Maybe Text)
batNextToken = lens _batNextToken (\s a -> s {_batNextToken = a})

-- | The maximum number of results to retrieve.
batMaxResults :: Lens' BatchListIndex (Maybe Natural)
batMaxResults = lens _batMaxResults (\s a -> s {_batMaxResults = a}) . mapping _Nat

-- | The reference to the index to list.
batIndexReference :: Lens' BatchListIndex ObjectReference
batIndexReference = lens _batIndexReference (\s a -> s {_batIndexReference = a})

instance Hashable BatchListIndex

instance NFData BatchListIndex

instance ToJSON BatchListIndex where
  toJSON BatchListIndex' {..} =
    object
      ( catMaybes
          [ ("RangesOnIndexedValues" .=) <$> _batRangesOnIndexedValues,
            ("NextToken" .=) <$> _batNextToken,
            ("MaxResults" .=) <$> _batMaxResults,
            Just ("IndexReference" .= _batIndexReference)
          ]
      )
