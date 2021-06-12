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
-- Module      : Network.AWS.SSM.Types.SignalType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SignalType
  ( SignalType
      ( ..,
        SignalType_Approve,
        SignalType_Reject,
        SignalType_Resume,
        SignalType_StartStep,
        SignalType_StopStep
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SignalType = SignalType'
  { fromSignalType ::
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

pattern SignalType_Approve :: SignalType
pattern SignalType_Approve = SignalType' "Approve"

pattern SignalType_Reject :: SignalType
pattern SignalType_Reject = SignalType' "Reject"

pattern SignalType_Resume :: SignalType
pattern SignalType_Resume = SignalType' "Resume"

pattern SignalType_StartStep :: SignalType
pattern SignalType_StartStep = SignalType' "StartStep"

pattern SignalType_StopStep :: SignalType
pattern SignalType_StopStep = SignalType' "StopStep"

{-# COMPLETE
  SignalType_Approve,
  SignalType_Reject,
  SignalType_Resume,
  SignalType_StartStep,
  SignalType_StopStep,
  SignalType'
  #-}
