{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndices where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists indices attached to an object inside a 'BatchRead' operation. For more information, see 'ListAttachedIndices' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListAttachedIndices' smart constructor.
data BatchListAttachedIndices = BatchListAttachedIndices'
  { _blaisNextToken ::
      !(Maybe Text),
    _blaisMaxResults :: !(Maybe Nat),
    _blaisTargetReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListAttachedIndices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blaisNextToken' - The pagination token.
--
-- * 'blaisMaxResults' - The maximum number of results to retrieve.
--
-- * 'blaisTargetReference' - A reference to the object that has indices attached.
batchListAttachedIndices ::
  -- | 'blaisTargetReference'
  ObjectReference ->
  BatchListAttachedIndices
batchListAttachedIndices pTargetReference_ =
  BatchListAttachedIndices'
    { _blaisNextToken = Nothing,
      _blaisMaxResults = Nothing,
      _blaisTargetReference = pTargetReference_
    }

-- | The pagination token.
blaisNextToken :: Lens' BatchListAttachedIndices (Maybe Text)
blaisNextToken = lens _blaisNextToken (\s a -> s {_blaisNextToken = a})

-- | The maximum number of results to retrieve.
blaisMaxResults :: Lens' BatchListAttachedIndices (Maybe Natural)
blaisMaxResults = lens _blaisMaxResults (\s a -> s {_blaisMaxResults = a}) . mapping _Nat

-- | A reference to the object that has indices attached.
blaisTargetReference :: Lens' BatchListAttachedIndices ObjectReference
blaisTargetReference = lens _blaisTargetReference (\s a -> s {_blaisTargetReference = a})

instance Hashable BatchListAttachedIndices

instance NFData BatchListAttachedIndices

instance ToJSON BatchListAttachedIndices where
  toJSON BatchListAttachedIndices' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _blaisNextToken,
            ("MaxResults" .=) <$> _blaisMaxResults,
            Just ("TargetReference" .= _blaisTargetReference)
          ]
      )
