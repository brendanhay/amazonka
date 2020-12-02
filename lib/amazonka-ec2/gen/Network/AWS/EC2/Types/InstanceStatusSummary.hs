{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusSummary where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceStatusDetails
import Network.AWS.EC2.Types.SummaryStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of an instance.
--
--
--
-- /See:/ 'instanceStatusSummary' smart constructor.
data InstanceStatusSummary = InstanceStatusSummary'
  { _issDetails ::
      !(Maybe [InstanceStatusDetails]),
    _issStatus :: !SummaryStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatusSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issDetails' - The system instance health or application instance health.
--
-- * 'issStatus' - The status.
instanceStatusSummary ::
  -- | 'issStatus'
  SummaryStatus ->
  InstanceStatusSummary
instanceStatusSummary pStatus_ =
  InstanceStatusSummary'
    { _issDetails = Nothing,
      _issStatus = pStatus_
    }

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\s a -> s {_issDetails = a}) . _Default . _Coerce

-- | The status.
issStatus :: Lens' InstanceStatusSummary SummaryStatus
issStatus = lens _issStatus (\s a -> s {_issStatus = a})

instance FromXML InstanceStatusSummary where
  parseXML x =
    InstanceStatusSummary'
      <$> (x .@? "details" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "status")

instance Hashable InstanceStatusSummary

instance NFData InstanceStatusSummary
