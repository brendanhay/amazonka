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
-- Module      : Amazonka.Lightsail.Types.BPAStatusMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.BPAStatusMessage
  ( BPAStatusMessage
      ( ..,
        BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING,
        BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING_ON_HOLD,
        BPAStatusMessage_SYNC_ON_HOLD,
        BPAStatusMessage_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BPAStatusMessage = BPAStatusMessage'
  { fromBPAStatusMessage ::
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

pattern BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING :: BPAStatusMessage
pattern BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING = BPAStatusMessage' "DEFAULTED_FOR_SLR_MISSING"

pattern BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING_ON_HOLD :: BPAStatusMessage
pattern BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING_ON_HOLD = BPAStatusMessage' "DEFAULTED_FOR_SLR_MISSING_ON_HOLD"

pattern BPAStatusMessage_SYNC_ON_HOLD :: BPAStatusMessage
pattern BPAStatusMessage_SYNC_ON_HOLD = BPAStatusMessage' "SYNC_ON_HOLD"

pattern BPAStatusMessage_Unknown :: BPAStatusMessage
pattern BPAStatusMessage_Unknown = BPAStatusMessage' "Unknown"

{-# COMPLETE
  BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING,
  BPAStatusMessage_DEFAULTED_FOR_SLR_MISSING_ON_HOLD,
  BPAStatusMessage_SYNC_ON_HOLD,
  BPAStatusMessage_Unknown,
  BPAStatusMessage'
  #-}
