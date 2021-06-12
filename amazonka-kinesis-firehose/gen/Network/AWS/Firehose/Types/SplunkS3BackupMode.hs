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
-- Module      : Network.AWS.Firehose.Types.SplunkS3BackupMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkS3BackupMode
  ( SplunkS3BackupMode
      ( ..,
        SplunkS3BackupMode_AllEvents,
        SplunkS3BackupMode_FailedEventsOnly
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SplunkS3BackupMode = SplunkS3BackupMode'
  { fromSplunkS3BackupMode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SplunkS3BackupMode_AllEvents :: SplunkS3BackupMode
pattern SplunkS3BackupMode_AllEvents = SplunkS3BackupMode' "AllEvents"

pattern SplunkS3BackupMode_FailedEventsOnly :: SplunkS3BackupMode
pattern SplunkS3BackupMode_FailedEventsOnly = SplunkS3BackupMode' "FailedEventsOnly"

{-# COMPLETE
  SplunkS3BackupMode_AllEvents,
  SplunkS3BackupMode_FailedEventsOnly,
  SplunkS3BackupMode'
  #-}
