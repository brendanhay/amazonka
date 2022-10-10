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
-- Module      : Amazonka.NetworkFirewall.Types.LogDestinationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.LogDestinationType
  ( LogDestinationType
      ( ..,
        LogDestinationType_CloudWatchLogs,
        LogDestinationType_KinesisDataFirehose,
        LogDestinationType_S3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LogDestinationType = LogDestinationType'
  { fromLogDestinationType ::
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

pattern LogDestinationType_CloudWatchLogs :: LogDestinationType
pattern LogDestinationType_CloudWatchLogs = LogDestinationType' "CloudWatchLogs"

pattern LogDestinationType_KinesisDataFirehose :: LogDestinationType
pattern LogDestinationType_KinesisDataFirehose = LogDestinationType' "KinesisDataFirehose"

pattern LogDestinationType_S3 :: LogDestinationType
pattern LogDestinationType_S3 = LogDestinationType' "S3"

{-# COMPLETE
  LogDestinationType_CloudWatchLogs,
  LogDestinationType_KinesisDataFirehose,
  LogDestinationType_S3,
  LogDestinationType'
  #-}
