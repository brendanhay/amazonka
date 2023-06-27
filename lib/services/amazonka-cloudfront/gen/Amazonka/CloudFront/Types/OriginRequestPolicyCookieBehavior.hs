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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyCookieBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyCookieBehavior
  ( OriginRequestPolicyCookieBehavior
      ( ..,
        OriginRequestPolicyCookieBehavior_All,
        OriginRequestPolicyCookieBehavior_AllExcept,
        OriginRequestPolicyCookieBehavior_None,
        OriginRequestPolicyCookieBehavior_Whitelist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OriginRequestPolicyCookieBehavior = OriginRequestPolicyCookieBehavior'
  { fromOriginRequestPolicyCookieBehavior ::
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

pattern OriginRequestPolicyCookieBehavior_All :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_All = OriginRequestPolicyCookieBehavior' "all"

pattern OriginRequestPolicyCookieBehavior_AllExcept :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_AllExcept = OriginRequestPolicyCookieBehavior' "allExcept"

pattern OriginRequestPolicyCookieBehavior_None :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_None = OriginRequestPolicyCookieBehavior' "none"

pattern OriginRequestPolicyCookieBehavior_Whitelist :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehavior_Whitelist = OriginRequestPolicyCookieBehavior' "whitelist"

{-# COMPLETE
  OriginRequestPolicyCookieBehavior_All,
  OriginRequestPolicyCookieBehavior_AllExcept,
  OriginRequestPolicyCookieBehavior_None,
  OriginRequestPolicyCookieBehavior_Whitelist,
  OriginRequestPolicyCookieBehavior'
  #-}
