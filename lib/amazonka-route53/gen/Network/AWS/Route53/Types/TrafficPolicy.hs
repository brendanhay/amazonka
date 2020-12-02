{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RecordType

-- | A complex type that contains settings for a traffic policy.
--
--
--
-- /See:/ 'trafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
  { _tpComment :: !(Maybe Text),
    _tpId :: !Text,
    _tpVersion :: !Nat,
    _tpName :: !Text,
    _tpType :: !RecordType,
    _tpDocument :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpComment' - The comment that you specify in the @CreateTrafficPolicy@ request, if any.
--
-- * 'tpId' - The ID that Amazon Route 53 assigned to a traffic policy when you created it.
--
-- * 'tpVersion' - The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
--
-- * 'tpName' - The name that you specified when you created the traffic policy.
--
-- * 'tpType' - The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- * 'tpDocument' - The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
trafficPolicy ::
  -- | 'tpId'
  Text ->
  -- | 'tpVersion'
  Natural ->
  -- | 'tpName'
  Text ->
  -- | 'tpType'
  RecordType ->
  -- | 'tpDocument'
  Text ->
  TrafficPolicy
trafficPolicy pId_ pVersion_ pName_ pType_ pDocument_ =
  TrafficPolicy'
    { _tpComment = Nothing,
      _tpId = pId_,
      _tpVersion = _Nat # pVersion_,
      _tpName = pName_,
      _tpType = pType_,
      _tpDocument = pDocument_
    }

-- | The comment that you specify in the @CreateTrafficPolicy@ request, if any.
tpComment :: Lens' TrafficPolicy (Maybe Text)
tpComment = lens _tpComment (\s a -> s {_tpComment = a})

-- | The ID that Amazon Route 53 assigned to a traffic policy when you created it.
tpId :: Lens' TrafficPolicy Text
tpId = lens _tpId (\s a -> s {_tpId = a})

-- | The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
tpVersion :: Lens' TrafficPolicy Natural
tpVersion = lens _tpVersion (\s a -> s {_tpVersion = a}) . _Nat

-- | The name that you specified when you created the traffic policy.
tpName :: Lens' TrafficPolicy Text
tpName = lens _tpName (\s a -> s {_tpName = a})

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
tpType :: Lens' TrafficPolicy RecordType
tpType = lens _tpType (\s a -> s {_tpType = a})

-- | The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
tpDocument :: Lens' TrafficPolicy Text
tpDocument = lens _tpDocument (\s a -> s {_tpDocument = a})

instance FromXML TrafficPolicy where
  parseXML x =
    TrafficPolicy'
      <$> (x .@? "Comment")
      <*> (x .@ "Id")
      <*> (x .@ "Version")
      <*> (x .@ "Name")
      <*> (x .@ "Type")
      <*> (x .@ "Document")

instance Hashable TrafficPolicy

instance NFData TrafficPolicy
