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
-- Module      : Network.AWS.Glue.Types.RecrawlBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlBehavior
  ( RecrawlBehavior
      ( ..,
        RecrawlBehavior_CRAWL_EVENT_MODE,
        RecrawlBehavior_CRAWL_EVERYTHING,
        RecrawlBehavior_CRAWL_NEW_FOLDERS_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RecrawlBehavior = RecrawlBehavior'
  { fromRecrawlBehavior ::
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

pattern RecrawlBehavior_CRAWL_EVENT_MODE :: RecrawlBehavior
pattern RecrawlBehavior_CRAWL_EVENT_MODE = RecrawlBehavior' "CRAWL_EVENT_MODE"

pattern RecrawlBehavior_CRAWL_EVERYTHING :: RecrawlBehavior
pattern RecrawlBehavior_CRAWL_EVERYTHING = RecrawlBehavior' "CRAWL_EVERYTHING"

pattern RecrawlBehavior_CRAWL_NEW_FOLDERS_ONLY :: RecrawlBehavior
pattern RecrawlBehavior_CRAWL_NEW_FOLDERS_ONLY = RecrawlBehavior' "CRAWL_NEW_FOLDERS_ONLY"

{-# COMPLETE
  RecrawlBehavior_CRAWL_EVENT_MODE,
  RecrawlBehavior_CRAWL_EVERYTHING,
  RecrawlBehavior_CRAWL_NEW_FOLDERS_ONLY,
  RecrawlBehavior'
  #-}
