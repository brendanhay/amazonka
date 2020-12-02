{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.StatusReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.StatusReport where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.
--
--
--
-- /See:/ 'statusReport' smart constructor.
data StatusReport = StatusReport'
  { _srStatus :: !(Maybe Text),
    _srCheckedTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatusReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
--
-- * 'srCheckedTime' - The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
statusReport ::
  StatusReport
statusReport =
  StatusReport' {_srStatus = Nothing, _srCheckedTime = Nothing}

-- | A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
srStatus :: Lens' StatusReport (Maybe Text)
srStatus = lens _srStatus (\s a -> s {_srStatus = a})

-- | The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
srCheckedTime :: Lens' StatusReport (Maybe UTCTime)
srCheckedTime = lens _srCheckedTime (\s a -> s {_srCheckedTime = a}) . mapping _Time

instance FromXML StatusReport where
  parseXML x =
    StatusReport' <$> (x .@? "Status") <*> (x .@? "CheckedTime")

instance Hashable StatusReport

instance NFData StatusReport
