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
-- Module      : Network.AWS.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateStatus
  ( OTAUpdateStatus
      ( ..,
        OTAUpdateStatus_CREATE_COMPLETE,
        OTAUpdateStatus_CREATE_FAILED,
        OTAUpdateStatus_CREATE_IN_PROGRESS,
        OTAUpdateStatus_CREATE_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OTAUpdateStatus = OTAUpdateStatus'
  { fromOTAUpdateStatus ::
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

pattern OTAUpdateStatus_CREATE_COMPLETE :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_COMPLETE = OTAUpdateStatus' "CREATE_COMPLETE"

pattern OTAUpdateStatus_CREATE_FAILED :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_FAILED = OTAUpdateStatus' "CREATE_FAILED"

pattern OTAUpdateStatus_CREATE_IN_PROGRESS :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_IN_PROGRESS = OTAUpdateStatus' "CREATE_IN_PROGRESS"

pattern OTAUpdateStatus_CREATE_PENDING :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_PENDING = OTAUpdateStatus' "CREATE_PENDING"

{-# COMPLETE
  OTAUpdateStatus_CREATE_COMPLETE,
  OTAUpdateStatus_CREATE_FAILED,
  OTAUpdateStatus_CREATE_IN_PROGRESS,
  OTAUpdateStatus_CREATE_PENDING,
  OTAUpdateStatus'
  #-}
