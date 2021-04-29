{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype HostedZoneLimitType = HostedZoneLimitType'
  { fromHostedZoneLimitType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
