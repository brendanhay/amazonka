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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
  ( OriginRequestPolicyCookieBehavior
      ( ..,
        OriginRequestPolicyCookieBehavior_All,
        OriginRequestPolicyCookieBehavior_None,
        OriginRequestPolicyCookieBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OriginRequestPolicyCookieBehavior = OriginRequestPolicyCookieBehavior'
  { fromOriginRequestPolicyCookieBehavior ::
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

pattern OriginRequestPolicyCookieBehavior_All :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_All = OriginRequestPolicyCookieBehavior' "all"

pattern OriginRequestPolicyCookieBehavior_None :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_None = OriginRequestPolicyCookieBehavior' "none"

pattern OriginRequestPolicyCookieBehavior_Whitelist :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_Whitelist = OriginRequestPolicyCookieBehavior' "whitelist"

{-# COMPLETE
  OriginRequestPolicyCookieBehavior_All,
  OriginRequestPolicyCookieBehavior_None,
  OriginRequestPolicyCookieBehavior_Whitelist,
  OriginRequestPolicyCookieBehavior'
  #-}
