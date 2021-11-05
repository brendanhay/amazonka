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
-- Module      : Network.AWS.IoTThingsGraph.Types.UploadStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTThingsGraph.Types.UploadStatus
  ( UploadStatus
      ( ..,
        UploadStatus_FAILED,
        UploadStatus_IN_PROGRESS,
        UploadStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UploadStatus = UploadStatus'
  { fromUploadStatus ::
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

pattern UploadStatus_FAILED :: UploadStatus
pattern UploadStatus_FAILED = UploadStatus' "FAILED"

pattern UploadStatus_IN_PROGRESS :: UploadStatus
pattern UploadStatus_IN_PROGRESS = UploadStatus' "IN_PROGRESS"

pattern UploadStatus_SUCCEEDED :: UploadStatus
pattern UploadStatus_SUCCEEDED = UploadStatus' "SUCCEEDED"

{-# COMPLETE
  UploadStatus_FAILED,
  UploadStatus_IN_PROGRESS,
  UploadStatus_SUCCEEDED,
  UploadStatus'
  #-}
