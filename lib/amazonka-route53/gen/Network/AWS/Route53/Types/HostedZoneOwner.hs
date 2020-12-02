{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneOwner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneOwner where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that identifies a hosted zone that a specified Amazon VPC is associated with and the owner of the hosted zone. If there is a value for @OwningAccount@ , there is no value for @OwningService@ , and vice versa.
--
--
--
-- /See:/ 'hostedZoneOwner' smart constructor.
data HostedZoneOwner = HostedZoneOwner'
  { _hzoOwningAccount ::
      !(Maybe Text),
    _hzoOwningService :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostedZoneOwner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzoOwningAccount' - If the hosted zone was created by an AWS account, or was created by an AWS service that creates hosted zones using the current account, @OwningAccount@ contains the account ID of that account. For example, when you use AWS Cloud Map to create a hosted zone, Cloud Map creates the hosted zone using the current AWS account.
--
-- * 'hzoOwningService' - If an AWS service uses its own account to create a hosted zone and associate the specified VPC with that hosted zone, @OwningService@ contains an abbreviation that identifies the service. For example, if Amazon Elastic File System (Amazon EFS) created a hosted zone and associated a VPC with the hosted zone, the value of @OwningService@ is @efs.amazonaws.com@ .
hostedZoneOwner ::
  HostedZoneOwner
hostedZoneOwner =
  HostedZoneOwner'
    { _hzoOwningAccount = Nothing,
      _hzoOwningService = Nothing
    }

-- | If the hosted zone was created by an AWS account, or was created by an AWS service that creates hosted zones using the current account, @OwningAccount@ contains the account ID of that account. For example, when you use AWS Cloud Map to create a hosted zone, Cloud Map creates the hosted zone using the current AWS account.
hzoOwningAccount :: Lens' HostedZoneOwner (Maybe Text)
hzoOwningAccount = lens _hzoOwningAccount (\s a -> s {_hzoOwningAccount = a})

-- | If an AWS service uses its own account to create a hosted zone and associate the specified VPC with that hosted zone, @OwningService@ contains an abbreviation that identifies the service. For example, if Amazon Elastic File System (Amazon EFS) created a hosted zone and associated a VPC with the hosted zone, the value of @OwningService@ is @efs.amazonaws.com@ .
hzoOwningService :: Lens' HostedZoneOwner (Maybe Text)
hzoOwningService = lens _hzoOwningService (\s a -> s {_hzoOwningService = a})

instance FromXML HostedZoneOwner where
  parseXML x =
    HostedZoneOwner'
      <$> (x .@? "OwningAccount") <*> (x .@? "OwningService")

instance Hashable HostedZoneOwner

instance NFData HostedZoneOwner
