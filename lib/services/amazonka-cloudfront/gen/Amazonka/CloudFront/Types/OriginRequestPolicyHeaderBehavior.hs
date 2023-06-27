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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyHeaderBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyHeaderBehavior
  ( OriginRequestPolicyHeaderBehavior
      ( ..,
        OriginRequestPolicyHeaderBehavior_AllExcept,
        OriginRequestPolicyHeaderBehavior_AllViewer,
        OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront,
        OriginRequestPolicyHeaderBehavior_None,
        OriginRequestPolicyHeaderBehavior_Whitelist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OriginRequestPolicyHeaderBehavior = OriginRequestPolicyHeaderBehavior'
  { fromOriginRequestPolicyHeaderBehavior ::
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

pattern OriginRequestPolicyHeaderBehavior_AllExcept :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_AllExcept = OriginRequestPolicyHeaderBehavior' "allExcept"

pattern OriginRequestPolicyHeaderBehavior_AllViewer :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_AllViewer = OriginRequestPolicyHeaderBehavior' "allViewer"

pattern OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront = OriginRequestPolicyHeaderBehavior' "allViewerAndWhitelistCloudFront"

pattern OriginRequestPolicyHeaderBehavior_None :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_None = OriginRequestPolicyHeaderBehavior' "none"

pattern OriginRequestPolicyHeaderBehavior_Whitelist :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_Whitelist = OriginRequestPolicyHeaderBehavior' "whitelist"

{-# COMPLETE
  OriginRequestPolicyHeaderBehavior_AllExcept,
  OriginRequestPolicyHeaderBehavior_AllViewer,
  OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront,
  OriginRequestPolicyHeaderBehavior_None,
  OriginRequestPolicyHeaderBehavior_Whitelist,
  OriginRequestPolicyHeaderBehavior'
  #-}
