{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortCriteria where

import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TransformSortColumnType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The sorting criteria that are associated with the machine learning transform.
--
--
--
-- /See:/ 'transformSortCriteria' smart constructor.
data TransformSortCriteria = TransformSortCriteria'
  { _tscColumn ::
      !TransformSortColumnType,
    _tscSortDirection :: !SortDirectionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformSortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tscColumn' - The column to be used in the sorting criteria that are associated with the machine learning transform.
--
-- * 'tscSortDirection' - The sort direction to be used in the sorting criteria that are associated with the machine learning transform.
transformSortCriteria ::
  -- | 'tscColumn'
  TransformSortColumnType ->
  -- | 'tscSortDirection'
  SortDirectionType ->
  TransformSortCriteria
transformSortCriteria pColumn_ pSortDirection_ =
  TransformSortCriteria'
    { _tscColumn = pColumn_,
      _tscSortDirection = pSortDirection_
    }

-- | The column to be used in the sorting criteria that are associated with the machine learning transform.
tscColumn :: Lens' TransformSortCriteria TransformSortColumnType
tscColumn = lens _tscColumn (\s a -> s {_tscColumn = a})

-- | The sort direction to be used in the sorting criteria that are associated with the machine learning transform.
tscSortDirection :: Lens' TransformSortCriteria SortDirectionType
tscSortDirection = lens _tscSortDirection (\s a -> s {_tscSortDirection = a})

instance Hashable TransformSortCriteria

instance NFData TransformSortCriteria

instance ToJSON TransformSortCriteria where
  toJSON TransformSortCriteria' {..} =
    object
      ( catMaybes
          [ Just ("Column" .= _tscColumn),
            Just ("SortDirection" .= _tscSortDirection)
          ]
      )
