{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a 'BatchRead' operation. For more information, see 'ListObjectParentPaths' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListObjectParentPaths' smart constructor.
data BatchListObjectParentPaths = BatchListObjectParentPaths'
  { _bloppsNextToken ::
      !(Maybe Text),
    _bloppsMaxResults :: !(Maybe Nat),
    _bloppsObjectReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectParentPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloppsNextToken' - The pagination token.
--
-- * 'bloppsMaxResults' - The maximum number of results to retrieve.
--
-- * 'bloppsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListObjectParentPaths ::
  -- | 'bloppsObjectReference'
  ObjectReference ->
  BatchListObjectParentPaths
batchListObjectParentPaths pObjectReference_ =
  BatchListObjectParentPaths'
    { _bloppsNextToken = Nothing,
      _bloppsMaxResults = Nothing,
      _bloppsObjectReference = pObjectReference_
    }

-- | The pagination token.
bloppsNextToken :: Lens' BatchListObjectParentPaths (Maybe Text)
bloppsNextToken = lens _bloppsNextToken (\s a -> s {_bloppsNextToken = a})

-- | The maximum number of results to retrieve.
bloppsMaxResults :: Lens' BatchListObjectParentPaths (Maybe Natural)
bloppsMaxResults = lens _bloppsMaxResults (\s a -> s {_bloppsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
bloppsObjectReference :: Lens' BatchListObjectParentPaths ObjectReference
bloppsObjectReference = lens _bloppsObjectReference (\s a -> s {_bloppsObjectReference = a})

instance Hashable BatchListObjectParentPaths

instance NFData BatchListObjectParentPaths

instance ToJSON BatchListObjectParentPaths where
  toJSON BatchListObjectParentPaths' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _bloppsNextToken,
            ("MaxResults" .=) <$> _bloppsMaxResults,
            Just ("ObjectReference" .= _bloppsObjectReference)
          ]
      )
