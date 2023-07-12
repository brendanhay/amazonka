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
-- Module      : Amazonka.Glue.Types.LastCrawlStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.LastCrawlStatus
  ( LastCrawlStatus
      ( ..,
        LastCrawlStatus_CANCELLED,
        LastCrawlStatus_FAILED,
        LastCrawlStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LastCrawlStatus = LastCrawlStatus'
  { fromLastCrawlStatus ::
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
