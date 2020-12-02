{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParents where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'batchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
  { _bloplNextToken ::
      !(Maybe Text),
    _bloplMaxResults :: !(Maybe Nat),
    _bloplObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectParents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloplNextToken' - Undocumented member.
--
-- * 'bloplMaxResults' - Undocumented member.
--
-- * 'bloplObjectReference' - Undocumented member.
batchListObjectParents ::
  -- | 'bloplObjectReference'
  ObjectReference ->
  BatchListObjectParents
batchListObjectParents pObjectReference_ =
  BatchListObjectParents'
    { _bloplNextToken = Nothing,
      _bloplMaxResults = Nothing,
      _bloplObjectReference = pObjectReference_
    }

-- | Undocumented member.
bloplNextToken :: Lens' BatchListObjectParents (Maybe Text)
bloplNextToken = lens _bloplNextToken (\s a -> s {_bloplNextToken = a})

-- | Undocumented member.
bloplMaxResults :: Lens' BatchListObjectParents (Maybe Natural)
bloplMaxResults = lens _bloplMaxResults (\s a -> s {_bloplMaxResults = a}) . mapping _Nat

-- | Undocumented member.
bloplObjectReference :: Lens' BatchListObjectParents ObjectReference
bloplObjectReference = lens _bloplObjectReference (\s a -> s {_bloplObjectReference = a})

instance Hashable BatchListObjectParents

instance NFData BatchListObjectParents

instance ToJSON BatchListObjectParents where
  toJSON BatchListObjectParents' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _bloplNextToken,
            ("MaxResults" .=) <$> _bloplMaxResults,
            Just ("ObjectReference" .= _bloplObjectReference)
          ]
      )
