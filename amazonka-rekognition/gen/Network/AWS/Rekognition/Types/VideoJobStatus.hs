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
-- Module      : Network.AWS.Rekognition.Types.VideoJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.VideoJobStatus
  ( VideoJobStatus
      ( ..,
        VideoJobStatus_FAILED,
        VideoJobStatus_IN_PROGRESS,
        VideoJobStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VideoJobStatus = VideoJobStatus'
  { fromVideoJobStatus ::
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

pattern VideoJobStatus_FAILED :: VideoJobStatus
pattern VideoJobStatus_FAILED = VideoJobStatus' "FAILED"

pattern VideoJobStatus_IN_PROGRESS :: VideoJobStatus
pattern VideoJobStatus_IN_PROGRESS = VideoJobStatus' "IN_PROGRESS"

pattern VideoJobStatus_SUCCEEDED :: VideoJobStatus
pattern VideoJobStatus_SUCCEEDED = VideoJobStatus' "SUCCEEDED"

{-# COMPLETE
  VideoJobStatus_FAILED,
  VideoJobStatus_IN_PROGRESS,
  VideoJobStatus_SUCCEEDED,
  VideoJobStatus'
  #-}
