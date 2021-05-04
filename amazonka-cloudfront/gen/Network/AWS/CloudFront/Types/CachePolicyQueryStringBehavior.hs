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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
  ( CachePolicyQueryStringBehavior
      ( ..,
        CachePolicyQueryStringBehavior_All,
        CachePolicyQueryStringBehavior_AllExcept,
        CachePolicyQueryStringBehavior_None,
        CachePolicyQueryStringBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CachePolicyQueryStringBehavior = CachePolicyQueryStringBehavior'
  { fromCachePolicyQueryStringBehavior ::
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

pattern CachePolicyQueryStringBehavior_All :: CachePolicyQueryStringBehavior
pattern CachePolicyQueryStringBehavior_All = CachePolicyQueryStringBehavior' "all"

pattern CachePolicyQueryStringBehavior_AllExcept :: CachePolicyQueryStringBehavior
pattern CachePolicyQueryStringBehavior_AllExcept = CachePolicyQueryStringBehavior' "allExcept"

pattern CachePolicyQueryStringBehavior_None :: CachePolicyQueryStringBehavior
pattern CachePolicyQueryStringBehavior_None = CachePolicyQueryStringBehavior' "none"

pattern CachePolicyQueryStringBehavior_Whitelist :: CachePolicyQueryStringBehavior
pattern CachePolicyQueryStringBehavior_Whitelist = CachePolicyQueryStringBehavior' "whitelist"

{-# COMPLETE
  CachePolicyQueryStringBehavior_All,
  CachePolicyQueryStringBehavior_AllExcept,
  CachePolicyQueryStringBehavior_None,
  CachePolicyQueryStringBehavior_Whitelist,
  CachePolicyQueryStringBehavior'
  #-}
