{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneLimit where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'hostedZoneLimit' smart constructor.
data HostedZoneLimit = HostedZoneLimit'
  { _hzlType ::
      !HostedZoneLimitType,
    _hzlValue :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostedZoneLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzlType' - The limit that you requested. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
-- * 'hzlValue' - The current value for the limit that is specified by @Type@ .
hostedZoneLimit ::
  -- | 'hzlType'
  HostedZoneLimitType ->
  -- | 'hzlValue'
  Natural ->
  HostedZoneLimit
hostedZoneLimit pType_ pValue_ =
  HostedZoneLimit' {_hzlType = pType_, _hzlValue = _Nat # pValue_}

-- | The limit that you requested. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
hzlType :: Lens' HostedZoneLimit HostedZoneLimitType
hzlType = lens _hzlType (\s a -> s {_hzlType = a})

-- | The current value for the limit that is specified by @Type@ .
hzlValue :: Lens' HostedZoneLimit Natural
hzlValue = lens _hzlValue (\s a -> s {_hzlValue = a}) . _Nat

instance FromXML HostedZoneLimit where
  parseXML x = HostedZoneLimit' <$> (x .@ "Type") <*> (x .@ "Value")

instance Hashable HostedZoneLimit

instance NFData HostedZoneLimit
