{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SearchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SearchStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
--
--
-- /See:/ 'searchStatus' smart constructor.
data SearchStatus = SearchStatus'
  { _sRid :: !(Maybe Text),
    _sTimems :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sRid' - The encrypted resource ID for the request.
--
-- * 'sTimems' - How long it took to process the request, in milliseconds.
searchStatus ::
  SearchStatus
searchStatus = SearchStatus' {_sRid = Nothing, _sTimems = Nothing}

-- | The encrypted resource ID for the request.
sRid :: Lens' SearchStatus (Maybe Text)
sRid = lens _sRid (\s a -> s {_sRid = a})

-- | How long it took to process the request, in milliseconds.
sTimems :: Lens' SearchStatus (Maybe Integer)
sTimems = lens _sTimems (\s a -> s {_sTimems = a})

instance FromJSON SearchStatus where
  parseJSON =
    withObject
      "SearchStatus"
      (\x -> SearchStatus' <$> (x .:? "rid") <*> (x .:? "timems"))

instance Hashable SearchStatus

instance NFData SearchStatus
