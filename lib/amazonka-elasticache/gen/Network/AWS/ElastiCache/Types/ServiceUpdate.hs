{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdate where

import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An update that you can apply to your Redis clusters.
--
--
--
-- /See:/ 'serviceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { _suEngineVersion ::
      !(Maybe Text),
    _suServiceUpdateType :: !(Maybe ServiceUpdateType),
    _suServiceUpdateName :: !(Maybe Text),
    _suEngine :: !(Maybe Text),
    _suServiceUpdateReleaseDate :: !(Maybe ISO8601),
    _suAutoUpdateAfterRecommendedApplyByDate :: !(Maybe Bool),
    _suServiceUpdateSeverity :: !(Maybe ServiceUpdateSeverity),
    _suServiceUpdateEndDate :: !(Maybe ISO8601),
    _suServiceUpdateDescription :: !(Maybe Text),
    _suServiceUpdateRecommendedApplyByDate :: !(Maybe ISO8601),
    _suServiceUpdateStatus :: !(Maybe ServiceUpdateStatus),
    _suEstimatedUpdateTime :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'suEngineVersion' - The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
--
-- * 'suServiceUpdateType' - Reflects the nature of the service update
--
-- * 'suServiceUpdateName' - The unique ID of the service update
--
-- * 'suEngine' - The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- * 'suServiceUpdateReleaseDate' - The date when the service update is initially available
--
-- * 'suAutoUpdateAfterRecommendedApplyByDate' - Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
--
-- * 'suServiceUpdateSeverity' - The severity of the service update
--
-- * 'suServiceUpdateEndDate' - The date after which the service update is no longer available
--
-- * 'suServiceUpdateDescription' - Provides details of the service update
--
-- * 'suServiceUpdateRecommendedApplyByDate' - The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
--
-- * 'suServiceUpdateStatus' - The status of the service update
--
-- * 'suEstimatedUpdateTime' - The estimated length of time the service update will take
serviceUpdate ::
  ServiceUpdate
serviceUpdate =
  ServiceUpdate'
    { _suEngineVersion = Nothing,
      _suServiceUpdateType = Nothing,
      _suServiceUpdateName = Nothing,
      _suEngine = Nothing,
      _suServiceUpdateReleaseDate = Nothing,
      _suAutoUpdateAfterRecommendedApplyByDate = Nothing,
      _suServiceUpdateSeverity = Nothing,
      _suServiceUpdateEndDate = Nothing,
      _suServiceUpdateDescription = Nothing,
      _suServiceUpdateRecommendedApplyByDate = Nothing,
      _suServiceUpdateStatus = Nothing,
      _suEstimatedUpdateTime = Nothing
    }

-- | The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
suEngineVersion :: Lens' ServiceUpdate (Maybe Text)
suEngineVersion = lens _suEngineVersion (\s a -> s {_suEngineVersion = a})

-- | Reflects the nature of the service update
suServiceUpdateType :: Lens' ServiceUpdate (Maybe ServiceUpdateType)
suServiceUpdateType = lens _suServiceUpdateType (\s a -> s {_suServiceUpdateType = a})

-- | The unique ID of the service update
suServiceUpdateName :: Lens' ServiceUpdate (Maybe Text)
suServiceUpdateName = lens _suServiceUpdateName (\s a -> s {_suServiceUpdateName = a})

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
suEngine :: Lens' ServiceUpdate (Maybe Text)
suEngine = lens _suEngine (\s a -> s {_suEngine = a})

-- | The date when the service update is initially available
suServiceUpdateReleaseDate :: Lens' ServiceUpdate (Maybe UTCTime)
suServiceUpdateReleaseDate = lens _suServiceUpdateReleaseDate (\s a -> s {_suServiceUpdateReleaseDate = a}) . mapping _Time

-- | Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
suAutoUpdateAfterRecommendedApplyByDate :: Lens' ServiceUpdate (Maybe Bool)
suAutoUpdateAfterRecommendedApplyByDate = lens _suAutoUpdateAfterRecommendedApplyByDate (\s a -> s {_suAutoUpdateAfterRecommendedApplyByDate = a})

-- | The severity of the service update
suServiceUpdateSeverity :: Lens' ServiceUpdate (Maybe ServiceUpdateSeverity)
suServiceUpdateSeverity = lens _suServiceUpdateSeverity (\s a -> s {_suServiceUpdateSeverity = a})

-- | The date after which the service update is no longer available
suServiceUpdateEndDate :: Lens' ServiceUpdate (Maybe UTCTime)
suServiceUpdateEndDate = lens _suServiceUpdateEndDate (\s a -> s {_suServiceUpdateEndDate = a}) . mapping _Time

-- | Provides details of the service update
suServiceUpdateDescription :: Lens' ServiceUpdate (Maybe Text)
suServiceUpdateDescription = lens _suServiceUpdateDescription (\s a -> s {_suServiceUpdateDescription = a})

-- | The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
suServiceUpdateRecommendedApplyByDate :: Lens' ServiceUpdate (Maybe UTCTime)
suServiceUpdateRecommendedApplyByDate = lens _suServiceUpdateRecommendedApplyByDate (\s a -> s {_suServiceUpdateRecommendedApplyByDate = a}) . mapping _Time

-- | The status of the service update
suServiceUpdateStatus :: Lens' ServiceUpdate (Maybe ServiceUpdateStatus)
suServiceUpdateStatus = lens _suServiceUpdateStatus (\s a -> s {_suServiceUpdateStatus = a})

-- | The estimated length of time the service update will take
suEstimatedUpdateTime :: Lens' ServiceUpdate (Maybe Text)
suEstimatedUpdateTime = lens _suEstimatedUpdateTime (\s a -> s {_suEstimatedUpdateTime = a})

instance FromXML ServiceUpdate where
  parseXML x =
    ServiceUpdate'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "ServiceUpdateType")
      <*> (x .@? "ServiceUpdateName")
      <*> (x .@? "Engine")
      <*> (x .@? "ServiceUpdateReleaseDate")
      <*> (x .@? "AutoUpdateAfterRecommendedApplyByDate")
      <*> (x .@? "ServiceUpdateSeverity")
      <*> (x .@? "ServiceUpdateEndDate")
      <*> (x .@? "ServiceUpdateDescription")
      <*> (x .@? "ServiceUpdateRecommendedApplyByDate")
      <*> (x .@? "ServiceUpdateStatus")
      <*> (x .@? "EstimatedUpdateTime")

instance Hashable ServiceUpdate

instance NFData ServiceUpdate
