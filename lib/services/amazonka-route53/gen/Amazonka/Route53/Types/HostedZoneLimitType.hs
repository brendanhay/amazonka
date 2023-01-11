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
-- Module      : Amazonka.Route53.Types.HostedZoneLimitType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HostedZoneLimitType
  ( HostedZoneLimitType
      ( ..,
        HostedZoneLimitType_MAX_RRSETS_BY_ZONE,
        HostedZoneLimitType_MAX_VPCS_ASSOCIATED_BY_ZONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype HostedZoneLimitType = HostedZoneLimitType'
  { fromHostedZoneLimitType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
