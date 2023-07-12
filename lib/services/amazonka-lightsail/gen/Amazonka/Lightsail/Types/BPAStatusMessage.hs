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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BPAStatusMessage = BPAStatusMessage'
  { fromBPAStatusMessage ::
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
