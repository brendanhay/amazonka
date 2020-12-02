{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildren where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectChildren' operation.
--
--
--
-- /See:/ 'batchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { _bloclNextToken ::
      !(Maybe Text),
    _bloclMaxResults :: !(Maybe Nat),
    _bloclObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloclNextToken' - The pagination token.
--
-- * 'bloclMaxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'bloclObjectReference' - Reference of the object for which child objects are being listed.
batchListObjectChildren ::
  -- | 'bloclObjectReference'
  ObjectReference ->
  BatchListObjectChildren
batchListObjectChildren pObjectReference_ =
  BatchListObjectChildren'
    { _bloclNextToken = Nothing,
      _bloclMaxResults = Nothing,
      _bloclObjectReference = pObjectReference_
    }

-- | The pagination token.
bloclNextToken :: Lens' BatchListObjectChildren (Maybe Text)
bloclNextToken = lens _bloclNextToken (\s a -> s {_bloclNextToken = a})

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
bloclMaxResults :: Lens' BatchListObjectChildren (Maybe Natural)
bloclMaxResults = lens _bloclMaxResults (\s a -> s {_bloclMaxResults = a}) . mapping _Nat

-- | Reference of the object for which child objects are being listed.
bloclObjectReference :: Lens' BatchListObjectChildren ObjectReference
bloclObjectReference = lens _bloclObjectReference (\s a -> s {_bloclObjectReference = a})

instance Hashable BatchListObjectChildren

instance NFData BatchListObjectChildren

instance ToJSON BatchListObjectChildren where
  toJSON BatchListObjectChildren' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _bloclNextToken,
            ("MaxResults" .=) <$> _bloclMaxResults,
            Just ("ObjectReference" .= _bloclObjectReference)
          ]
      )
