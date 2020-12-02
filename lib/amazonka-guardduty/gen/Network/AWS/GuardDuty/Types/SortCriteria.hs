{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.SortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.SortCriteria where

import Network.AWS.GuardDuty.Types.OrderBy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the criteria used for sorting findings.
--
--
--
-- /See:/ 'sortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { _scOrderBy :: !(Maybe OrderBy),
    _scAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scOrderBy' - The order by which the sorted findings are to be displayed.
--
-- * 'scAttributeName' - Represents the finding attribute (for example, accountId) to sort findings by.
sortCriteria ::
  SortCriteria
sortCriteria =
  SortCriteria' {_scOrderBy = Nothing, _scAttributeName = Nothing}

-- | The order by which the sorted findings are to be displayed.
scOrderBy :: Lens' SortCriteria (Maybe OrderBy)
scOrderBy = lens _scOrderBy (\s a -> s {_scOrderBy = a})

-- | Represents the finding attribute (for example, accountId) to sort findings by.
scAttributeName :: Lens' SortCriteria (Maybe Text)
scAttributeName = lens _scAttributeName (\s a -> s {_scAttributeName = a})

instance Hashable SortCriteria

instance NFData SortCriteria

instance ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    object
      ( catMaybes
          [ ("orderBy" .=) <$> _scOrderBy,
            ("attributeName" .=) <$> _scAttributeName
          ]
      )
