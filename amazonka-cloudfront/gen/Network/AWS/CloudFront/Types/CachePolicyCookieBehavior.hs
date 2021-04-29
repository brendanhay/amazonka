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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
  ( CachePolicyCookieBehavior
      ( ..,
        CachePolicyCookieBehavior_All,
        CachePolicyCookieBehavior_AllExcept,
        CachePolicyCookieBehavior_None,
        CachePolicyCookieBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CachePolicyCookieBehavior = CachePolicyCookieBehavior'
  { fromCachePolicyCookieBehavior ::
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

pattern CachePolicyCookieBehavior_All :: CachePolicyCookieBehavior
pattern CachePolicyCookieBehavior_All = CachePolicyCookieBehavior' "all"

pattern CachePolicyCookieBehavior_AllExcept :: CachePolicyCookieBehavior
pattern CachePolicyCookieBehavior_AllExcept = CachePolicyCookieBehavior' "allExcept"

pattern CachePolicyCookieBehavior_None :: CachePolicyCookieBehavior
pattern CachePolicyCookieBehavior_None = CachePolicyCookieBehavior' "none"

pattern CachePolicyCookieBehavior_Whitelist :: CachePolicyCookieBehavior
pattern CachePolicyCookieBehavior_Whitelist = CachePolicyCookieBehavior' "whitelist"

{-# COMPLETE
  CachePolicyCookieBehavior_All,
  CachePolicyCookieBehavior_AllExcept,
  CachePolicyCookieBehavior_None,
  CachePolicyCookieBehavior_Whitelist,
  CachePolicyCookieBehavior'
  #-}
