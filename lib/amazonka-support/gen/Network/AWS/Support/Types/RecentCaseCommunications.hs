{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.RecentCaseCommunications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.RecentCaseCommunications where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.Communication

-- | The five most recent communications associated with the case.
--
--
--
-- /See:/ 'recentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
  { _rccNextToken ::
      !(Maybe Text),
    _rccCommunications ::
      !(Maybe [Communication])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecentCaseCommunications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccNextToken' - A resumption point for pagination.
--
-- * 'rccCommunications' - The five most recent communications associated with the case.
recentCaseCommunications ::
  RecentCaseCommunications
recentCaseCommunications =
  RecentCaseCommunications'
    { _rccNextToken = Nothing,
      _rccCommunications = Nothing
    }

-- | A resumption point for pagination.
rccNextToken :: Lens' RecentCaseCommunications (Maybe Text)
rccNextToken = lens _rccNextToken (\s a -> s {_rccNextToken = a})

-- | The five most recent communications associated with the case.
rccCommunications :: Lens' RecentCaseCommunications [Communication]
rccCommunications = lens _rccCommunications (\s a -> s {_rccCommunications = a}) . _Default . _Coerce

instance FromJSON RecentCaseCommunications where
  parseJSON =
    withObject
      "RecentCaseCommunications"
      ( \x ->
          RecentCaseCommunications'
            <$> (x .:? "nextToken") <*> (x .:? "communications" .!= mempty)
      )

instance Hashable RecentCaseCommunications

instance NFData RecentCaseCommunications
