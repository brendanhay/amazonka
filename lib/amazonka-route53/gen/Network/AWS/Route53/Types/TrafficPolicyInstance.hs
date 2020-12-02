{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicyInstance where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RecordType

-- | A complex type that contains settings for the new traffic policy instance.
--
--
--
-- /See:/ 'trafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
  { _tpiId ::
      !Text,
    _tpiHostedZoneId :: !ResourceId,
    _tpiName :: !Text,
    _tpiTTL :: !Nat,
    _tpiState :: !Text,
    _tpiMessage :: !Text,
    _tpiTrafficPolicyId :: !Text,
    _tpiTrafficPolicyVersion :: !Nat,
    _tpiTrafficPolicyType :: !RecordType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficPolicyInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpiId' - The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- * 'tpiHostedZoneId' - The ID of the hosted zone that Amazon Route 53 created resource record sets in.
--
-- * 'tpiName' - The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
--
-- * 'tpiTTL' - The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
--
-- * 'tpiState' - The value of @State@ is one of the following values:     * Applied    * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.     * Creating    * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.     * Failed    * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
--
-- * 'tpiMessage' - If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
--
-- * 'tpiTrafficPolicyId' - The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- * 'tpiTrafficPolicyVersion' - The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- * 'tpiTrafficPolicyType' - The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
trafficPolicyInstance ::
  -- | 'tpiId'
  Text ->
  -- | 'tpiHostedZoneId'
  ResourceId ->
  -- | 'tpiName'
  Text ->
  -- | 'tpiTTL'
  Natural ->
  -- | 'tpiState'
  Text ->
  -- | 'tpiMessage'
  Text ->
  -- | 'tpiTrafficPolicyId'
  Text ->
  -- | 'tpiTrafficPolicyVersion'
  Natural ->
  -- | 'tpiTrafficPolicyType'
  RecordType ->
  TrafficPolicyInstance
trafficPolicyInstance
  pId_
  pHostedZoneId_
  pName_
  pTTL_
  pState_
  pMessage_
  pTrafficPolicyId_
  pTrafficPolicyVersion_
  pTrafficPolicyType_ =
    TrafficPolicyInstance'
      { _tpiId = pId_,
        _tpiHostedZoneId = pHostedZoneId_,
        _tpiName = pName_,
        _tpiTTL = _Nat # pTTL_,
        _tpiState = pState_,
        _tpiMessage = pMessage_,
        _tpiTrafficPolicyId = pTrafficPolicyId_,
        _tpiTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_,
        _tpiTrafficPolicyType = pTrafficPolicyType_
      }

-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
tpiId :: Lens' TrafficPolicyInstance Text
tpiId = lens _tpiId (\s a -> s {_tpiId = a})

-- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
tpiHostedZoneId :: Lens' TrafficPolicyInstance ResourceId
tpiHostedZoneId = lens _tpiHostedZoneId (\s a -> s {_tpiHostedZoneId = a})

-- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
tpiName :: Lens' TrafficPolicyInstance Text
tpiName = lens _tpiName (\s a -> s {_tpiName = a})

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
tpiTTL :: Lens' TrafficPolicyInstance Natural
tpiTTL = lens _tpiTTL (\s a -> s {_tpiTTL = a}) . _Nat

-- | The value of @State@ is one of the following values:     * Applied    * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.     * Creating    * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.     * Failed    * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
tpiState :: Lens' TrafficPolicyInstance Text
tpiState = lens _tpiState (\s a -> s {_tpiState = a})

-- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
tpiMessage :: Lens' TrafficPolicyInstance Text
tpiMessage = lens _tpiMessage (\s a -> s {_tpiMessage = a})

-- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
tpiTrafficPolicyId :: Lens' TrafficPolicyInstance Text
tpiTrafficPolicyId = lens _tpiTrafficPolicyId (\s a -> s {_tpiTrafficPolicyId = a})

-- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
tpiTrafficPolicyVersion :: Lens' TrafficPolicyInstance Natural
tpiTrafficPolicyVersion = lens _tpiTrafficPolicyVersion (\s a -> s {_tpiTrafficPolicyVersion = a}) . _Nat

-- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
tpiTrafficPolicyType :: Lens' TrafficPolicyInstance RecordType
tpiTrafficPolicyType = lens _tpiTrafficPolicyType (\s a -> s {_tpiTrafficPolicyType = a})

instance FromXML TrafficPolicyInstance where
  parseXML x =
    TrafficPolicyInstance'
      <$> (x .@ "Id")
      <*> (x .@ "HostedZoneId")
      <*> (x .@ "Name")
      <*> (x .@ "TTL")
      <*> (x .@ "State")
      <*> (x .@ "Message")
      <*> (x .@ "TrafficPolicyId")
      <*> (x .@ "TrafficPolicyVersion")
      <*> (x .@ "TrafficPolicyType")

instance Hashable TrafficPolicyInstance

instance NFData TrafficPolicyInstance
