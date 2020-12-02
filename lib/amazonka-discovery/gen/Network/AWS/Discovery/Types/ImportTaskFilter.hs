{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilter where

import Network.AWS.Discovery.Types.ImportTaskFilterName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A name-values pair of elements you can use to filter the results when querying your import tasks. Currently, wildcards are not supported for filters.
--
--
--
-- /See:/ 'importTaskFilter' smart constructor.
data ImportTaskFilter = ImportTaskFilter'
  { _itfValues ::
      !(Maybe (List1 Text)),
    _itfName :: !(Maybe ImportTaskFilterName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportTaskFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itfValues' - An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
--
-- * 'itfName' - The name, status, or import task ID for a specific import task.
importTaskFilter ::
  ImportTaskFilter
importTaskFilter =
  ImportTaskFilter' {_itfValues = Nothing, _itfName = Nothing}

-- | An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
itfValues :: Lens' ImportTaskFilter (Maybe (NonEmpty Text))
itfValues = lens _itfValues (\s a -> s {_itfValues = a}) . mapping _List1

-- | The name, status, or import task ID for a specific import task.
itfName :: Lens' ImportTaskFilter (Maybe ImportTaskFilterName)
itfName = lens _itfName (\s a -> s {_itfName = a})

instance Hashable ImportTaskFilter

instance NFData ImportTaskFilter

instance ToJSON ImportTaskFilter where
  toJSON ImportTaskFilter' {..} =
    object
      ( catMaybes
          [("values" .=) <$> _itfValues, ("name" .=) <$> _itfName]
      )
