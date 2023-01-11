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
-- Module      : Amazonka.CloudFront.Types.CachePolicyCookieBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicyCookieBehavior
  ( CachePolicyCookieBehavior
      ( ..,
        CachePolicyCookieBehavior_All,
        CachePolicyCookieBehavior_AllExcept,
        CachePolicyCookieBehavior_None,
        CachePolicyCookieBehavior_Whitelist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CachePolicyCookieBehavior = CachePolicyCookieBehavior'
  { fromCachePolicyCookieBehavior ::
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
