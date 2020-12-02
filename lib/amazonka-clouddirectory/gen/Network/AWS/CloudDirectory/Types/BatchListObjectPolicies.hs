{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { _bbNextToken ::
      !(Maybe Text),
    _bbMaxResults :: !(Maybe Nat),
    _bbObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbNextToken' - The pagination token.
--
-- * 'bbMaxResults' - The maximum number of results to retrieve.
--
-- * 'bbObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListObjectPolicies ::
  -- | 'bbObjectReference'
  ObjectReference ->
  BatchListObjectPolicies
batchListObjectPolicies pObjectReference_ =
  BatchListObjectPolicies'
    { _bbNextToken = Nothing,
      _bbMaxResults = Nothing,
      _bbObjectReference = pObjectReference_
    }

-- | The pagination token.
bbNextToken :: Lens' BatchListObjectPolicies (Maybe Text)
bbNextToken = lens _bbNextToken (\s a -> s {_bbNextToken = a})

-- | The maximum number of results to retrieve.
bbMaxResults :: Lens' BatchListObjectPolicies (Maybe Natural)
bbMaxResults = lens _bbMaxResults (\s a -> s {_bbMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
bbObjectReference :: Lens' BatchListObjectPolicies ObjectReference
bbObjectReference = lens _bbObjectReference (\s a -> s {_bbObjectReference = a})

instance Hashable BatchListObjectPolicies

instance NFData BatchListObjectPolicies

instance ToJSON BatchListObjectPolicies where
  toJSON BatchListObjectPolicies' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _bbNextToken,
            ("MaxResults" .=) <$> _bbMaxResults,
            Just ("ObjectReference" .= _bbObjectReference)
          ]
      )
