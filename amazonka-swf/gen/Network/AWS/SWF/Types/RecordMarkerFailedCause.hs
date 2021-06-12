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
-- Module      : Network.AWS.SWF.Types.RecordMarkerFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RecordMarkerFailedCause
  ( RecordMarkerFailedCause
      ( ..,
        RecordMarkerFailedCause_OPERATION_NOT_PERMITTED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RecordMarkerFailedCause = RecordMarkerFailedCause'
  { fromRecordMarkerFailedCause ::
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

pattern RecordMarkerFailedCause_OPERATION_NOT_PERMITTED :: RecordMarkerFailedCause
pattern RecordMarkerFailedCause_OPERATION_NOT_PERMITTED = RecordMarkerFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  RecordMarkerFailedCause_OPERATION_NOT_PERMITTED,
  RecordMarkerFailedCause'
  #-}
