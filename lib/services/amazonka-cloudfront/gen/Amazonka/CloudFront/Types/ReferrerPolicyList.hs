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
-- Module      : Amazonka.CloudFront.Types.ReferrerPolicyList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ReferrerPolicyList
  ( ReferrerPolicyList
      ( ..,
        ReferrerPolicyList_No_referrer,
        ReferrerPolicyList_No_referrer_when_downgrade,
        ReferrerPolicyList_Origin,
        ReferrerPolicyList_Origin_when_cross_origin,
        ReferrerPolicyList_Same_origin,
        ReferrerPolicyList_Strict_origin,
        ReferrerPolicyList_Strict_origin_when_cross_origin,
        ReferrerPolicyList_Unsafe_url
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReferrerPolicyList = ReferrerPolicyList'
  { fromReferrerPolicyList ::
      Core.Text
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

pattern ReferrerPolicyList_No_referrer :: ReferrerPolicyList
pattern ReferrerPolicyList_No_referrer = ReferrerPolicyList' "no-referrer"

pattern ReferrerPolicyList_No_referrer_when_downgrade :: ReferrerPolicyList
pattern ReferrerPolicyList_No_referrer_when_downgrade = ReferrerPolicyList' "no-referrer-when-downgrade"

pattern ReferrerPolicyList_Origin :: ReferrerPolicyList
pattern ReferrerPolicyList_Origin = ReferrerPolicyList' "origin"

pattern ReferrerPolicyList_Origin_when_cross_origin :: ReferrerPolicyList
pattern ReferrerPolicyList_Origin_when_cross_origin = ReferrerPolicyList' "origin-when-cross-origin"

pattern ReferrerPolicyList_Same_origin :: ReferrerPolicyList
pattern ReferrerPolicyList_Same_origin = ReferrerPolicyList' "same-origin"

pattern ReferrerPolicyList_Strict_origin :: ReferrerPolicyList
pattern ReferrerPolicyList_Strict_origin = ReferrerPolicyList' "strict-origin"

pattern ReferrerPolicyList_Strict_origin_when_cross_origin :: ReferrerPolicyList
pattern ReferrerPolicyList_Strict_origin_when_cross_origin = ReferrerPolicyList' "strict-origin-when-cross-origin"

pattern ReferrerPolicyList_Unsafe_url :: ReferrerPolicyList
pattern ReferrerPolicyList_Unsafe_url = ReferrerPolicyList' "unsafe-url"

{-# COMPLETE
  ReferrerPolicyList_No_referrer,
  ReferrerPolicyList_No_referrer_when_downgrade,
  ReferrerPolicyList_Origin,
  ReferrerPolicyList_Origin_when_cross_origin,
  ReferrerPolicyList_Same_origin,
  ReferrerPolicyList_Strict_origin,
  ReferrerPolicyList_Strict_origin_when_cross_origin,
  ReferrerPolicyList_Unsafe_url,
  ReferrerPolicyList'
  #-}
