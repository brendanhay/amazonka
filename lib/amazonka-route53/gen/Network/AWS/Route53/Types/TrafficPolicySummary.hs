{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicySummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RecordType

-- | A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.
--
--
--
-- /See:/ 'trafficPolicySummary' smart constructor.
data TrafficPolicySummary = TrafficPolicySummary'
  { _tpsId :: !Text,
    _tpsName :: !Text,
    _tpsType :: !RecordType,
    _tpsLatestVersion :: !Nat,
    _tpsTrafficPolicyCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficPolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpsId' - The ID that Amazon Route 53 assigned to the traffic policy when you created it.
--
-- * 'tpsName' - The name that you specified for the traffic policy when you created it.
--
-- * 'tpsType' - The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- * 'tpsLatestVersion' - The version number of the latest version of the traffic policy.
--
-- * 'tpsTrafficPolicyCount' - The number of traffic policies that are associated with the current AWS account.
trafficPolicySummary ::
  -- | 'tpsId'
  Text ->
  -- | 'tpsName'
  Text ->
  -- | 'tpsType'
  RecordType ->
  -- | 'tpsLatestVersion'
  Natural ->
  -- | 'tpsTrafficPolicyCount'
  Natural ->
  TrafficPolicySummary
trafficPolicySummary
  pId_
  pName_
  pType_
  pLatestVersion_
  pTrafficPolicyCount_ =
    TrafficPolicySummary'
      { _tpsId = pId_,
        _tpsName = pName_,
        _tpsType = pType_,
        _tpsLatestVersion = _Nat # pLatestVersion_,
        _tpsTrafficPolicyCount = _Nat # pTrafficPolicyCount_
      }

-- | The ID that Amazon Route 53 assigned to the traffic policy when you created it.
tpsId :: Lens' TrafficPolicySummary Text
tpsId = lens _tpsId (\s a -> s {_tpsId = a})

-- | The name that you specified for the traffic policy when you created it.
tpsName :: Lens' TrafficPolicySummary Text
tpsName = lens _tpsName (\s a -> s {_tpsName = a})

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
tpsType :: Lens' TrafficPolicySummary RecordType
tpsType = lens _tpsType (\s a -> s {_tpsType = a})

-- | The version number of the latest version of the traffic policy.
tpsLatestVersion :: Lens' TrafficPolicySummary Natural
tpsLatestVersion = lens _tpsLatestVersion (\s a -> s {_tpsLatestVersion = a}) . _Nat

-- | The number of traffic policies that are associated with the current AWS account.
tpsTrafficPolicyCount :: Lens' TrafficPolicySummary Natural
tpsTrafficPolicyCount = lens _tpsTrafficPolicyCount (\s a -> s {_tpsTrafficPolicyCount = a}) . _Nat

instance FromXML TrafficPolicySummary where
  parseXML x =
    TrafficPolicySummary'
      <$> (x .@ "Id")
      <*> (x .@ "Name")
      <*> (x .@ "Type")
      <*> (x .@ "LatestVersion")
      <*> (x .@ "TrafficPolicyCount")

instance Hashable TrafficPolicySummary

instance NFData TrafficPolicySummary
