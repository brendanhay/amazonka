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
-- Module      : Amazonka.VoiceId.Types.SpeakerStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.SpeakerStatus
  ( SpeakerStatus
      ( ..,
        SpeakerStatus_ENROLLED,
        SpeakerStatus_EXPIRED,
        SpeakerStatus_OPTED_OUT,
        SpeakerStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SpeakerStatus = SpeakerStatus'
  { fromSpeakerStatus ::
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

pattern SpeakerStatus_ENROLLED :: SpeakerStatus
pattern SpeakerStatus_ENROLLED = SpeakerStatus' "ENROLLED"

pattern SpeakerStatus_EXPIRED :: SpeakerStatus
pattern SpeakerStatus_EXPIRED = SpeakerStatus' "EXPIRED"

pattern SpeakerStatus_OPTED_OUT :: SpeakerStatus
pattern SpeakerStatus_OPTED_OUT = SpeakerStatus' "OPTED_OUT"

pattern SpeakerStatus_PENDING :: SpeakerStatus
pattern SpeakerStatus_PENDING = SpeakerStatus' "PENDING"

{-# COMPLETE
  SpeakerStatus_ENROLLED,
  SpeakerStatus_EXPIRED,
  SpeakerStatus_OPTED_OUT,
  SpeakerStatus_PENDING,
  SpeakerStatus'
  #-}
