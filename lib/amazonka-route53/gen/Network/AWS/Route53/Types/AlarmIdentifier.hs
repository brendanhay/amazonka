{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AlarmIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AlarmIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.CloudWatchRegion

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
--
--
-- /See:/ 'alarmIdentifier' smart constructor.
data AlarmIdentifier = AlarmIdentifier'
  { _aiRegion ::
      !CloudWatchRegion,
    _aiName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlarmIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiRegion' - For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in. For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
--
-- * 'aiName' - The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
alarmIdentifier ::
  -- | 'aiRegion'
  CloudWatchRegion ->
  -- | 'aiName'
  Text ->
  AlarmIdentifier
alarmIdentifier pRegion_ pName_ =
  AlarmIdentifier' {_aiRegion = pRegion_, _aiName = pName_}

-- | For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in. For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
aiRegion :: Lens' AlarmIdentifier CloudWatchRegion
aiRegion = lens _aiRegion (\s a -> s {_aiRegion = a})

-- | The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
aiName :: Lens' AlarmIdentifier Text
aiName = lens _aiName (\s a -> s {_aiName = a})

instance FromXML AlarmIdentifier where
  parseXML x = AlarmIdentifier' <$> (x .@ "Region") <*> (x .@ "Name")

instance Hashable AlarmIdentifier

instance NFData AlarmIdentifier

instance ToXML AlarmIdentifier where
  toXML AlarmIdentifier' {..} =
    mconcat ["Region" @= _aiRegion, "Name" @= _aiName]
