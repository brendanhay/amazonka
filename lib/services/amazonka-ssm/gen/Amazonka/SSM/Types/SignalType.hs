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
-- Module      : Amazonka.SSM.Types.SignalType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.SignalType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SignalType = SignalType'
  { fromSignalType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
