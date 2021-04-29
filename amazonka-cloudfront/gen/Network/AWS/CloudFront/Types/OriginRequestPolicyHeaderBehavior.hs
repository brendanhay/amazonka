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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
  ( OriginRequestPolicyHeaderBehavior
      ( ..,
        OriginRequestPolicyHeaderBehavior_AllViewer,
        OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront,
        OriginRequestPolicyHeaderBehavior_None,
        OriginRequestPolicyHeaderBehavior_Whitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OriginRequestPolicyHeaderBehavior = OriginRequestPolicyHeaderBehavior'
  { fromOriginRequestPolicyHeaderBehavior ::
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

pattern OriginRequestPolicyHeaderBehavior_AllViewer :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_AllViewer = OriginRequestPolicyHeaderBehavior' "allViewer"

pattern OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront = OriginRequestPolicyHeaderBehavior' "allViewerAndWhitelistCloudFront"

pattern OriginRequestPolicyHeaderBehavior_None :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_None = OriginRequestPolicyHeaderBehavior' "none"

pattern OriginRequestPolicyHeaderBehavior_Whitelist :: OriginRequestPolicyHeaderBehavior
pattern OriginRequestPolicyHeaderBehavior_Whitelist = OriginRequestPolicyHeaderBehavior' "whitelist"

{-# COMPLETE
  OriginRequestPolicyHeaderBehavior_AllViewer,
  OriginRequestPolicyHeaderBehavior_AllViewerAndWhitelistCloudFront,
  OriginRequestPolicyHeaderBehavior_None,
  OriginRequestPolicyHeaderBehavior_Whitelist,
  OriginRequestPolicyHeaderBehavior'
  #-}
