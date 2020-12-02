{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProvisionedBandwidth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProvisionedBandwidth where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
--
--
-- /See:/ 'provisionedBandwidth' smart constructor.
data ProvisionedBandwidth = ProvisionedBandwidth'
  { _pbStatus ::
      !(Maybe Text),
    _pbRequested :: !(Maybe Text),
    _pbProvisioned :: !(Maybe Text),
    _pbRequestTime :: !(Maybe ISO8601),
    _pbProvisionTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedBandwidth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbStatus' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbRequested' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbProvisioned' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbRequestTime' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbProvisionTime' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
provisionedBandwidth ::
  ProvisionedBandwidth
provisionedBandwidth =
  ProvisionedBandwidth'
    { _pbStatus = Nothing,
      _pbRequested = Nothing,
      _pbProvisioned = Nothing,
      _pbRequestTime = Nothing,
      _pbProvisionTime = Nothing
    }

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbStatus :: Lens' ProvisionedBandwidth (Maybe Text)
pbStatus = lens _pbStatus (\s a -> s {_pbStatus = a})

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbRequested :: Lens' ProvisionedBandwidth (Maybe Text)
pbRequested = lens _pbRequested (\s a -> s {_pbRequested = a})

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbProvisioned :: Lens' ProvisionedBandwidth (Maybe Text)
pbProvisioned = lens _pbProvisioned (\s a -> s {_pbProvisioned = a})

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbRequestTime :: Lens' ProvisionedBandwidth (Maybe UTCTime)
pbRequestTime = lens _pbRequestTime (\s a -> s {_pbRequestTime = a}) . mapping _Time

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbProvisionTime :: Lens' ProvisionedBandwidth (Maybe UTCTime)
pbProvisionTime = lens _pbProvisionTime (\s a -> s {_pbProvisionTime = a}) . mapping _Time

instance FromXML ProvisionedBandwidth where
  parseXML x =
    ProvisionedBandwidth'
      <$> (x .@? "status")
      <*> (x .@? "requested")
      <*> (x .@? "provisioned")
      <*> (x .@? "requestTime")
      <*> (x .@? "provisionTime")

instance Hashable ProvisionedBandwidth

instance NFData ProvisionedBandwidth
