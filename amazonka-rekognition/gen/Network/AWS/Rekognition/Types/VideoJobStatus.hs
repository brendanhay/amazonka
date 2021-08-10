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
import qualified Network.AWS.Prelude as Prelude

newtype VideoJobStatus = VideoJobStatus'
  { fromVideoJobStatus ::
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
