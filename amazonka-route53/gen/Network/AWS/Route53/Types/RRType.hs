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
-- Module      : Network.AWS.Route53.Types.RRType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.RRType
  ( RRType
      ( ..,
        RRType_A,
        RRType_AAAA,
        RRType_CAA,
        RRType_CNAME,
        RRType_DS,
        RRType_MX,
        RRType_NAPTR,
        RRType_NS,
        RRType_PTR,
        RRType_SOA,
        RRType_SPF,
        RRType_SRV,
        RRType_TXT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype RRType = RRType' {fromRRType :: Core.Text}
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

pattern RRType_A :: RRType
pattern RRType_A = RRType' "A"

pattern RRType_AAAA :: RRType
pattern RRType_AAAA = RRType' "AAAA"

pattern RRType_CAA :: RRType
pattern RRType_CAA = RRType' "CAA"

pattern RRType_CNAME :: RRType
pattern RRType_CNAME = RRType' "CNAME"

pattern RRType_DS :: RRType
pattern RRType_DS = RRType' "DS"

pattern RRType_MX :: RRType
pattern RRType_MX = RRType' "MX"

pattern RRType_NAPTR :: RRType
pattern RRType_NAPTR = RRType' "NAPTR"

pattern RRType_NS :: RRType
pattern RRType_NS = RRType' "NS"

pattern RRType_PTR :: RRType
pattern RRType_PTR = RRType' "PTR"

pattern RRType_SOA :: RRType
pattern RRType_SOA = RRType' "SOA"

pattern RRType_SPF :: RRType
pattern RRType_SPF = RRType' "SPF"

pattern RRType_SRV :: RRType
pattern RRType_SRV = RRType' "SRV"

pattern RRType_TXT :: RRType
pattern RRType_TXT = RRType' "TXT"

{-# COMPLETE
  RRType_A,
  RRType_AAAA,
  RRType_CAA,
  RRType_CNAME,
  RRType_DS,
  RRType_MX,
  RRType_NAPTR,
  RRType_NS,
  RRType_PTR,
  RRType_SOA,
  RRType_SPF,
  RRType_SRV,
  RRType_TXT,
  RRType'
  #-}
