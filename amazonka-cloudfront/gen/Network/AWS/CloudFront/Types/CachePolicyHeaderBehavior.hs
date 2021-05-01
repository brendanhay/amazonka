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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
  ( CachePolicyHeaderBehavior
      ( ..,
        CachePolicyHeaderBehavior_None,
        CachePolicyHeaderBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CachePolicyHeaderBehavior = CachePolicyHeaderBehavior'
  { fromCachePolicyHeaderBehavior ::
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

pattern CachePolicyHeaderBehavior_None :: CachePolicyHeaderBehavior
pattern CachePolicyHeaderBehavior_None = CachePolicyHeaderBehavior' "none"

pattern CachePolicyHeaderBehavior_Whitelist :: CachePolicyHeaderBehavior
pattern CachePolicyHeaderBehavior_Whitelist = CachePolicyHeaderBehavior' "whitelist"

{-# COMPLETE
  CachePolicyHeaderBehavior_None,
  CachePolicyHeaderBehavior_Whitelist,
  CachePolicyHeaderBehavior'
  #-}
