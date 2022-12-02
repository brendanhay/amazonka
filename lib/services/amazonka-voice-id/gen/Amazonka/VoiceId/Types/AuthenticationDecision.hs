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
-- Module      : Amazonka.VoiceId.Types.AuthenticationDecision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.AuthenticationDecision
  ( AuthenticationDecision
      ( ..,
        AuthenticationDecision_ACCEPT,
        AuthenticationDecision_NOT_ENOUGH_SPEECH,
        AuthenticationDecision_REJECT,
        AuthenticationDecision_SPEAKER_EXPIRED,
        AuthenticationDecision_SPEAKER_ID_NOT_PROVIDED,
        AuthenticationDecision_SPEAKER_NOT_ENROLLED,
        AuthenticationDecision_SPEAKER_OPTED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticationDecision = AuthenticationDecision'
  { fromAuthenticationDecision ::
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

pattern AuthenticationDecision_ACCEPT :: AuthenticationDecision
pattern AuthenticationDecision_ACCEPT = AuthenticationDecision' "ACCEPT"

pattern AuthenticationDecision_NOT_ENOUGH_SPEECH :: AuthenticationDecision
pattern AuthenticationDecision_NOT_ENOUGH_SPEECH = AuthenticationDecision' "NOT_ENOUGH_SPEECH"

pattern AuthenticationDecision_REJECT :: AuthenticationDecision
pattern AuthenticationDecision_REJECT = AuthenticationDecision' "REJECT"

pattern AuthenticationDecision_SPEAKER_EXPIRED :: AuthenticationDecision
pattern AuthenticationDecision_SPEAKER_EXPIRED = AuthenticationDecision' "SPEAKER_EXPIRED"

pattern AuthenticationDecision_SPEAKER_ID_NOT_PROVIDED :: AuthenticationDecision
pattern AuthenticationDecision_SPEAKER_ID_NOT_PROVIDED = AuthenticationDecision' "SPEAKER_ID_NOT_PROVIDED"

pattern AuthenticationDecision_SPEAKER_NOT_ENROLLED :: AuthenticationDecision
pattern AuthenticationDecision_SPEAKER_NOT_ENROLLED = AuthenticationDecision' "SPEAKER_NOT_ENROLLED"

pattern AuthenticationDecision_SPEAKER_OPTED_OUT :: AuthenticationDecision
pattern AuthenticationDecision_SPEAKER_OPTED_OUT = AuthenticationDecision' "SPEAKER_OPTED_OUT"

{-# COMPLETE
  AuthenticationDecision_ACCEPT,
  AuthenticationDecision_NOT_ENOUGH_SPEECH,
  AuthenticationDecision_REJECT,
  AuthenticationDecision_SPEAKER_EXPIRED,
  AuthenticationDecision_SPEAKER_ID_NOT_PROVIDED,
  AuthenticationDecision_SPEAKER_NOT_ENROLLED,
  AuthenticationDecision_SPEAKER_OPTED_OUT,
  AuthenticationDecision'
  #-}
