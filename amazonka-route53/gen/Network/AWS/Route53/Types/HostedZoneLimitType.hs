{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneLimitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneLimitType
  ( HostedZoneLimitType
      ( ..,
        HostedZoneLimitType_MAX_RRSETS_BY_ZONE,
        HostedZoneLimitType_MAX_VPCS_ASSOCIATED_BY_ZONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Route53.Internal

newtype HostedZoneLimitType = HostedZoneLimitType'
  { fromHostedZoneLimitType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern HostedZoneLimitType_MAX_RRSETS_BY_ZONE :: HostedZoneLimitType
pattern HostedZoneLimitType_MAX_RRSETS_BY_ZONE = HostedZoneLimitType' "MAX_RRSETS_BY_ZONE"

pattern HostedZoneLimitType_MAX_VPCS_ASSOCIATED_BY_ZONE :: HostedZoneLimitType
pattern HostedZoneLimitType_MAX_VPCS_ASSOCIATED_BY_ZONE = HostedZoneLimitType' "MAX_VPCS_ASSOCIATED_BY_ZONE"

{-# COMPLETE
  HostedZoneLimitType_MAX_RRSETS_BY_ZONE,
  HostedZoneLimitType_MAX_VPCS_ASSOCIATED_BY_ZONE,
  HostedZoneLimitType'
  #-}
