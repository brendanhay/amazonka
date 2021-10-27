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
-- Module      : Network.AWS.Kendra.Types.WebCrawlerMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.WebCrawlerMode
  ( WebCrawlerMode
      ( ..,
        WebCrawlerMode_EVERYTHING,
        WebCrawlerMode_HOST_ONLY,
        WebCrawlerMode_SUBDOMAINS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype WebCrawlerMode = WebCrawlerMode'
  { fromWebCrawlerMode ::
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

pattern WebCrawlerMode_EVERYTHING :: WebCrawlerMode
pattern WebCrawlerMode_EVERYTHING = WebCrawlerMode' "EVERYTHING"

pattern WebCrawlerMode_HOST_ONLY :: WebCrawlerMode
pattern WebCrawlerMode_HOST_ONLY = WebCrawlerMode' "HOST_ONLY"

pattern WebCrawlerMode_SUBDOMAINS :: WebCrawlerMode
pattern WebCrawlerMode_SUBDOMAINS = WebCrawlerMode' "SUBDOMAINS"

{-# COMPLETE
  WebCrawlerMode_EVERYTHING,
  WebCrawlerMode_HOST_ONLY,
  WebCrawlerMode_SUBDOMAINS,
  WebCrawlerMode'
  #-}
