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
-- Module      : Network.AWS.Glue.Types.CrawlerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerState
  ( CrawlerState
      ( ..,
        CrawlerState_READY,
        CrawlerState_RUNNING,
        CrawlerState_STOPPING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CrawlerState = CrawlerState'
  { fromCrawlerState ::
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

pattern CrawlerState_READY :: CrawlerState
pattern CrawlerState_READY = CrawlerState' "READY"

pattern CrawlerState_RUNNING :: CrawlerState
pattern CrawlerState_RUNNING = CrawlerState' "RUNNING"

pattern CrawlerState_STOPPING :: CrawlerState
pattern CrawlerState_STOPPING = CrawlerState' "STOPPING"

{-# COMPLETE
  CrawlerState_READY,
  CrawlerState_RUNNING,
  CrawlerState_STOPPING,
  CrawlerState'
  #-}
