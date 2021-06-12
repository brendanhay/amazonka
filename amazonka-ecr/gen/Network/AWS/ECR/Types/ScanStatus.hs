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
-- Module      : Network.AWS.ECR.Types.ScanStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ScanStatus
  ( ScanStatus
      ( ..,
        ScanStatus_COMPLETE,
        ScanStatus_FAILED,
        ScanStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScanStatus = ScanStatus'
  { fromScanStatus ::
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

pattern ScanStatus_COMPLETE :: ScanStatus
pattern ScanStatus_COMPLETE = ScanStatus' "COMPLETE"

pattern ScanStatus_FAILED :: ScanStatus
pattern ScanStatus_FAILED = ScanStatus' "FAILED"

pattern ScanStatus_IN_PROGRESS :: ScanStatus
pattern ScanStatus_IN_PROGRESS = ScanStatus' "IN_PROGRESS"

{-# COMPLETE
  ScanStatus_COMPLETE,
  ScanStatus_FAILED,
  ScanStatus_IN_PROGRESS,
  ScanStatus'
  #-}
