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
-- Module      : Amazonka.CloudFront.Types.CachePolicyHeaderBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicyHeaderBehavior
  ( CachePolicyHeaderBehavior
      ( ..,
        CachePolicyHeaderBehavior_None,
        CachePolicyHeaderBehavior_Whitelist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CachePolicyHeaderBehavior = CachePolicyHeaderBehavior'
  { fromCachePolicyHeaderBehavior ::
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

pattern CachePolicyHeaderBehavior_None :: CachePolicyHeaderBehavior
pattern CachePolicyHeaderBehavior_None = CachePolicyHeaderBehavior' "none"

pattern CachePolicyHeaderBehavior_Whitelist :: CachePolicyHeaderBehavior
pattern CachePolicyHeaderBehavior_Whitelist = CachePolicyHeaderBehavior' "whitelist"

{-# COMPLETE
  CachePolicyHeaderBehavior_None,
  CachePolicyHeaderBehavior_Whitelist,
  CachePolicyHeaderBehavior'
  #-}
