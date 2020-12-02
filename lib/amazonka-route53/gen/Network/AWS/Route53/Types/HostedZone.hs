{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZone where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains general information about the hosted zone.
--
--
--
-- /See:/ 'hostedZone' smart constructor.
data HostedZone = HostedZone'
  { _hzLinkedService ::
      !(Maybe LinkedService),
    _hzConfig :: !(Maybe HostedZoneConfig),
    _hzResourceRecordSetCount :: !(Maybe Integer),
    _hzId :: !ResourceId,
    _hzName :: !Text,
    _hzCallerReference :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzLinkedService' - If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53.
--
-- * 'hzConfig' - A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
--
-- * 'hzResourceRecordSetCount' - The number of resource record sets in the hosted zone.
--
-- * 'hzId' - The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- * 'hzName' - The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
--
-- * 'hzCallerReference' - The value that you specified for @CallerReference@ when you created the hosted zone.
hostedZone ::
  -- | 'hzId'
  ResourceId ->
  -- | 'hzName'
  Text ->
  -- | 'hzCallerReference'
  Text ->
  HostedZone
hostedZone pId_ pName_ pCallerReference_ =
  HostedZone'
    { _hzLinkedService = Nothing,
      _hzConfig = Nothing,
      _hzResourceRecordSetCount = Nothing,
      _hzId = pId_,
      _hzName = pName_,
      _hzCallerReference = pCallerReference_
    }

-- | If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53.
hzLinkedService :: Lens' HostedZone (Maybe LinkedService)
hzLinkedService = lens _hzLinkedService (\s a -> s {_hzLinkedService = a})

-- | A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
hzConfig :: Lens' HostedZone (Maybe HostedZoneConfig)
hzConfig = lens _hzConfig (\s a -> s {_hzConfig = a})

-- | The number of resource record sets in the hosted zone.
hzResourceRecordSetCount :: Lens' HostedZone (Maybe Integer)
hzResourceRecordSetCount = lens _hzResourceRecordSetCount (\s a -> s {_hzResourceRecordSetCount = a})

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
hzId :: Lens' HostedZone ResourceId
hzId = lens _hzId (\s a -> s {_hzId = a})

-- | The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
hzName :: Lens' HostedZone Text
hzName = lens _hzName (\s a -> s {_hzName = a})

-- | The value that you specified for @CallerReference@ when you created the hosted zone.
hzCallerReference :: Lens' HostedZone Text
hzCallerReference = lens _hzCallerReference (\s a -> s {_hzCallerReference = a})

instance FromXML HostedZone where
  parseXML x =
    HostedZone'
      <$> (x .@? "LinkedService")
      <*> (x .@? "Config")
      <*> (x .@? "ResourceRecordSetCount")
      <*> (x .@ "Id")
      <*> (x .@ "Name")
      <*> (x .@ "CallerReference")

instance Hashable HostedZone

instance NFData HostedZone
