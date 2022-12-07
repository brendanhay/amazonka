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
-- Module      : Amazonka.CloudFront.Types.CachePolicyQueryStringBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicyQueryStringBehavior
  ( CachePolicyQueryStringBehavior
      ( ..,
        CachePolicyQueryStringBehavior_All,
        CachePolicyQueryStringBehavior_AllExcept,
        CachePolicyQueryStringBehavior_None,
        CachePolicyQueryStringBehavior_Whitelist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CachePolicyQueryStringBehavior = CachePolicyQueryStringBehavior'
  { fromCachePolicyQueryStringBehavior ::
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
