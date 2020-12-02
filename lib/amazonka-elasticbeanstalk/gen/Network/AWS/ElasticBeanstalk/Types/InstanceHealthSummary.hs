{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents summary information about the health of an instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
--
--
-- /See:/ 'instanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { _ihsOK ::
      !(Maybe Int),
    _ihsPending :: !(Maybe Int),
    _ihsSevere :: !(Maybe Int),
    _ihsUnknown :: !(Maybe Int),
    _ihsNoData :: !(Maybe Int),
    _ihsWarning :: !(Maybe Int),
    _ihsDegraded :: !(Maybe Int),
    _ihsInfo :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihsOK' - __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
--
-- * 'ihsPending' - __Grey.__ An operation is in progress on an instance within the command timeout.
--
-- * 'ihsSevere' - __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
--
-- * 'ihsUnknown' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
--
-- * 'ihsNoData' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
--
-- * 'ihsWarning' - __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
--
-- * 'ihsDegraded' - __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
--
-- * 'ihsInfo' - __Green.__ An operation is in progress on an instance.
instanceHealthSummary ::
  InstanceHealthSummary
instanceHealthSummary =
  InstanceHealthSummary'
    { _ihsOK = Nothing,
      _ihsPending = Nothing,
      _ihsSevere = Nothing,
      _ihsUnknown = Nothing,
      _ihsNoData = Nothing,
      _ihsWarning = Nothing,
      _ihsDegraded = Nothing,
      _ihsInfo = Nothing
    }

-- | __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
ihsOK :: Lens' InstanceHealthSummary (Maybe Int)
ihsOK = lens _ihsOK (\s a -> s {_ihsOK = a})

-- | __Grey.__ An operation is in progress on an instance within the command timeout.
ihsPending :: Lens' InstanceHealthSummary (Maybe Int)
ihsPending = lens _ihsPending (\s a -> s {_ihsPending = a})

-- | __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
ihsSevere :: Lens' InstanceHealthSummary (Maybe Int)
ihsSevere = lens _ihsSevere (\s a -> s {_ihsSevere = a})

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
ihsUnknown :: Lens' InstanceHealthSummary (Maybe Int)
ihsUnknown = lens _ihsUnknown (\s a -> s {_ihsUnknown = a})

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
ihsNoData :: Lens' InstanceHealthSummary (Maybe Int)
ihsNoData = lens _ihsNoData (\s a -> s {_ihsNoData = a})

-- | __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
ihsWarning :: Lens' InstanceHealthSummary (Maybe Int)
ihsWarning = lens _ihsWarning (\s a -> s {_ihsWarning = a})

-- | __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
ihsDegraded :: Lens' InstanceHealthSummary (Maybe Int)
ihsDegraded = lens _ihsDegraded (\s a -> s {_ihsDegraded = a})

-- | __Green.__ An operation is in progress on an instance.
ihsInfo :: Lens' InstanceHealthSummary (Maybe Int)
ihsInfo = lens _ihsInfo (\s a -> s {_ihsInfo = a})

instance FromXML InstanceHealthSummary where
  parseXML x =
    InstanceHealthSummary'
      <$> (x .@? "Ok")
      <*> (x .@? "Pending")
      <*> (x .@? "Severe")
      <*> (x .@? "Unknown")
      <*> (x .@? "NoData")
      <*> (x .@? "Warning")
      <*> (x .@? "Degraded")
      <*> (x .@? "Info")

instance Hashable InstanceHealthSummary

instance NFData InstanceHealthSummary
