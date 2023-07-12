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
-- Module      : Amazonka.Kendra.Types.WebCrawlerMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.WebCrawlerMode
  ( WebCrawlerMode
      ( ..,
        WebCrawlerMode_EVERYTHING,
        WebCrawlerMode_HOST_ONLY,
        WebCrawlerMode_SUBDOMAINS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WebCrawlerMode = WebCrawlerMode'
  { fromWebCrawlerMode ::
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
