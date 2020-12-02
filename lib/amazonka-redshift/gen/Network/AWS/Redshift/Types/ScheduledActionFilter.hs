{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduledActionFilterName

-- | A set of elements to filter the returned scheduled actions.
--
--
--
-- /See:/ 'scheduledActionFilter' smart constructor.
data ScheduledActionFilter = ScheduledActionFilter'
  { _safName ::
      !ScheduledActionFilterName,
    _safValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledActionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'safName' - The type of element to filter.
--
-- * 'safValues' - List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions.
scheduledActionFilter ::
  -- | 'safName'
  ScheduledActionFilterName ->
  ScheduledActionFilter
scheduledActionFilter pName_ =
  ScheduledActionFilter' {_safName = pName_, _safValues = mempty}

-- | The type of element to filter.
safName :: Lens' ScheduledActionFilter ScheduledActionFilterName
safName = lens _safName (\s a -> s {_safName = a})

-- | List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions.
safValues :: Lens' ScheduledActionFilter [Text]
safValues = lens _safValues (\s a -> s {_safValues = a}) . _Coerce

instance Hashable ScheduledActionFilter

instance NFData ScheduledActionFilter

instance ToQuery ScheduledActionFilter where
  toQuery ScheduledActionFilter' {..} =
    mconcat
      ["Name" =: _safName, "Values" =: toQueryList "item" _safValues]
