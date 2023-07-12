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
-- Module      : Amazonka.Glue.Types.RecrawlBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RecrawlBehavior
  ( RecrawlBehavior
      ( ..,
        RecrawlBehavior_CRAWL_EVENT_MODE,
        RecrawlBehavior_CRAWL_EVERYTHING,
        RecrawlBehavior_CRAWL_NEW_FOLDERS_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecrawlBehavior = RecrawlBehavior'
  { fromRecrawlBehavior ::
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
