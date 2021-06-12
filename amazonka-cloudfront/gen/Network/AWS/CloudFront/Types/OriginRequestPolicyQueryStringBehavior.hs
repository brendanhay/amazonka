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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
  ( OriginRequestPolicyQueryStringBehavior
      ( ..,
        OriginRequestPolicyQueryStringBehavior_All,
        OriginRequestPolicyQueryStringBehavior_None,
        OriginRequestPolicyQueryStringBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OriginRequestPolicyQueryStringBehavior = OriginRequestPolicyQueryStringBehavior'
  { fromOriginRequestPolicyQueryStringBehavior ::
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

pattern OriginRequestPolicyQueryStringBehavior_All :: OriginRequestPolicyQueryStringBehavior
pattern OriginRequestPolicyQueryStringBehavior_All = OriginRequestPolicyQueryStringBehavior' "all"

pattern OriginRequestPolicyQueryStringBehavior_None :: OriginRequestPolicyQueryStringBehavior
pattern OriginRequestPolicyQueryStringBehavior_None = OriginRequestPolicyQueryStringBehavior' "none"

pattern OriginRequestPolicyQueryStringBehavior_Whitelist :: OriginRequestPolicyQueryStringBehavior
pattern OriginRequestPolicyQueryStringBehavior_Whitelist = OriginRequestPolicyQueryStringBehavior' "whitelist"

{-# COMPLETE
  OriginRequestPolicyQueryStringBehavior_All,
  OriginRequestPolicyQueryStringBehavior_None,
  OriginRequestPolicyQueryStringBehavior_Whitelist,
  OriginRequestPolicyQueryStringBehavior'
  #-}
