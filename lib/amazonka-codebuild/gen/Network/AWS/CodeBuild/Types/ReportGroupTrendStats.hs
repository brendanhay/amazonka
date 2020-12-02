{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendStats where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'reportGroupTrendStats' smart constructor.
data ReportGroupTrendStats = ReportGroupTrendStats'
  { _rgtsMax ::
      !(Maybe Text),
    _rgtsAverage :: !(Maybe Text),
    _rgtsMin :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReportGroupTrendStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgtsMax' - Undocumented member.
--
-- * 'rgtsAverage' - Undocumented member.
--
-- * 'rgtsMin' - Undocumented member.
reportGroupTrendStats ::
  ReportGroupTrendStats
reportGroupTrendStats =
  ReportGroupTrendStats'
    { _rgtsMax = Nothing,
      _rgtsAverage = Nothing,
      _rgtsMin = Nothing
    }

-- | Undocumented member.
rgtsMax :: Lens' ReportGroupTrendStats (Maybe Text)
rgtsMax = lens _rgtsMax (\s a -> s {_rgtsMax = a})

-- | Undocumented member.
rgtsAverage :: Lens' ReportGroupTrendStats (Maybe Text)
rgtsAverage = lens _rgtsAverage (\s a -> s {_rgtsAverage = a})

-- | Undocumented member.
rgtsMin :: Lens' ReportGroupTrendStats (Maybe Text)
rgtsMin = lens _rgtsMin (\s a -> s {_rgtsMin = a})

instance FromJSON ReportGroupTrendStats where
  parseJSON =
    withObject
      "ReportGroupTrendStats"
      ( \x ->
          ReportGroupTrendStats'
            <$> (x .:? "max") <*> (x .:? "average") <*> (x .:? "min")
      )

instance Hashable ReportGroupTrendStats

instance NFData ReportGroupTrendStats
