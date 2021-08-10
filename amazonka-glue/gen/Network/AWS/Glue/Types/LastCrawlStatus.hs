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
-- Module      : Network.AWS.Glue.Types.LastCrawlStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LastCrawlStatus
  ( LastCrawlStatus
      ( ..,
        LastCrawlStatus_CANCELLED,
        LastCrawlStatus_FAILED,
        LastCrawlStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LastCrawlStatus = LastCrawlStatus'
  { fromLastCrawlStatus ::
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

pattern LastCrawlStatus_CANCELLED :: LastCrawlStatus
pattern LastCrawlStatus_CANCELLED = LastCrawlStatus' "CANCELLED"

pattern LastCrawlStatus_FAILED :: LastCrawlStatus
pattern LastCrawlStatus_FAILED = LastCrawlStatus' "FAILED"

pattern LastCrawlStatus_SUCCEEDED :: LastCrawlStatus
pattern LastCrawlStatus_SUCCEEDED = LastCrawlStatus' "SUCCEEDED"

{-# COMPLETE
  LastCrawlStatus_CANCELLED,
  LastCrawlStatus_FAILED,
  LastCrawlStatus_SUCCEEDED,
  LastCrawlStatus'
  #-}
