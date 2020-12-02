{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneOwner

-- | In the response to a @ListHostedZonesByVPC@ request, the @HostedZoneSummaries@ element contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
--
--
-- /See:/ 'hostedZoneSummary' smart constructor.
data HostedZoneSummary = HostedZoneSummary'
  { _hzsHostedZoneId ::
      !ResourceId,
    _hzsName :: !Text,
    _hzsOwner :: !HostedZoneOwner
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostedZoneSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzsHostedZoneId' - The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
--
-- * 'hzsName' - The name of the private hosted zone, such as @example.com@ .
--
-- * 'hzsOwner' - The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
hostedZoneSummary ::
  -- | 'hzsHostedZoneId'
  ResourceId ->
  -- | 'hzsName'
  Text ->
  -- | 'hzsOwner'
  HostedZoneOwner ->
  HostedZoneSummary
hostedZoneSummary pHostedZoneId_ pName_ pOwner_ =
  HostedZoneSummary'
    { _hzsHostedZoneId = pHostedZoneId_,
      _hzsName = pName_,
      _hzsOwner = pOwner_
    }

-- | The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
hzsHostedZoneId :: Lens' HostedZoneSummary ResourceId
hzsHostedZoneId = lens _hzsHostedZoneId (\s a -> s {_hzsHostedZoneId = a})

-- | The name of the private hosted zone, such as @example.com@ .
hzsName :: Lens' HostedZoneSummary Text
hzsName = lens _hzsName (\s a -> s {_hzsName = a})

-- | The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
hzsOwner :: Lens' HostedZoneSummary HostedZoneOwner
hzsOwner = lens _hzsOwner (\s a -> s {_hzsOwner = a})

instance FromXML HostedZoneSummary where
  parseXML x =
    HostedZoneSummary'
      <$> (x .@ "HostedZoneId") <*> (x .@ "Name") <*> (x .@ "Owner")

instance Hashable HostedZoneSummary

instance NFData HostedZoneSummary
