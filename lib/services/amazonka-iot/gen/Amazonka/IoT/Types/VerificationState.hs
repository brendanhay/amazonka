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
-- Module      : Amazonka.IoT.Types.VerificationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.VerificationState
  ( VerificationState
      ( ..,
        VerificationState_BENIGN_POSITIVE,
        VerificationState_FALSE_POSITIVE,
        VerificationState_TRUE_POSITIVE,
        VerificationState_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VerificationState = VerificationState'
  { fromVerificationState ::
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

pattern VerificationState_BENIGN_POSITIVE :: VerificationState
pattern VerificationState_BENIGN_POSITIVE = VerificationState' "BENIGN_POSITIVE"

pattern VerificationState_FALSE_POSITIVE :: VerificationState
pattern VerificationState_FALSE_POSITIVE = VerificationState' "FALSE_POSITIVE"

pattern VerificationState_TRUE_POSITIVE :: VerificationState
pattern VerificationState_TRUE_POSITIVE = VerificationState' "TRUE_POSITIVE"

pattern VerificationState_UNKNOWN :: VerificationState
pattern VerificationState_UNKNOWN = VerificationState' "UNKNOWN"

{-# COMPLETE
  VerificationState_BENIGN_POSITIVE,
  VerificationState_FALSE_POSITIVE,
  VerificationState_TRUE_POSITIVE,
  VerificationState_UNKNOWN,
  VerificationState'
  #-}
