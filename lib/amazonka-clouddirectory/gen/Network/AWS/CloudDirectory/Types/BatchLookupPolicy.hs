{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists all policies from the root of the Directory to the object specified inside a 'BatchRead' operation. For more information, see 'LookupPolicy' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { _blplNextToken ::
      !(Maybe Text),
    _blplMaxResults :: !(Maybe Nat),
    _blplObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchLookupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blplNextToken' - The pagination token.
--
-- * 'blplMaxResults' - The maximum number of results to retrieve.
--
-- * 'blplObjectReference' - Reference that identifies the object whose policies will be looked up.
batchLookupPolicy ::
  -- | 'blplObjectReference'
  ObjectReference ->
  BatchLookupPolicy
batchLookupPolicy pObjectReference_ =
  BatchLookupPolicy'
    { _blplNextToken = Nothing,
      _blplMaxResults = Nothing,
      _blplObjectReference = pObjectReference_
    }

-- | The pagination token.
blplNextToken :: Lens' BatchLookupPolicy (Maybe Text)
blplNextToken = lens _blplNextToken (\s a -> s {_blplNextToken = a})

-- | The maximum number of results to retrieve.
blplMaxResults :: Lens' BatchLookupPolicy (Maybe Natural)
blplMaxResults = lens _blplMaxResults (\s a -> s {_blplMaxResults = a}) . mapping _Nat

-- | Reference that identifies the object whose policies will be looked up.
blplObjectReference :: Lens' BatchLookupPolicy ObjectReference
blplObjectReference = lens _blplObjectReference (\s a -> s {_blplObjectReference = a})

instance Hashable BatchLookupPolicy

instance NFData BatchLookupPolicy

instance ToJSON BatchLookupPolicy where
  toJSON BatchLookupPolicy' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _blplNextToken,
            ("MaxResults" .=) <$> _blplMaxResults,
            Just ("ObjectReference" .= _blplObjectReference)
          ]
      )
