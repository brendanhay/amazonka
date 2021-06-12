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
-- Module      : Network.AWS.Lightsail.Types.RecordState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RecordState
  ( RecordState
      ( ..,
        RecordState_Failed,
        RecordState_Started,
        RecordState_Succeeded
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RecordState = RecordState'
  { fromRecordState ::
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

pattern RecordState_Failed :: RecordState
pattern RecordState_Failed = RecordState' "Failed"

pattern RecordState_Started :: RecordState
pattern RecordState_Started = RecordState' "Started"

pattern RecordState_Succeeded :: RecordState
pattern RecordState_Succeeded = RecordState' "Succeeded"

{-# COMPLETE
  RecordState_Failed,
  RecordState_Started,
  RecordState_Succeeded,
  RecordState'
  #-}
