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
-- Module      : Amazonka.ChimeSdkMeetings.Types.TranscribePartialResultsStability
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.TranscribePartialResultsStability
  ( TranscribePartialResultsStability
      ( ..,
        TranscribePartialResultsStability_High,
        TranscribePartialResultsStability_Low,
        TranscribePartialResultsStability_Medium
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribePartialResultsStability = TranscribePartialResultsStability'
  { fromTranscribePartialResultsStability ::
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

pattern TranscribePartialResultsStability_High :: TranscribePartialResultsStability
pattern TranscribePartialResultsStability_High = TranscribePartialResultsStability' "high"

pattern TranscribePartialResultsStability_Low :: TranscribePartialResultsStability
pattern TranscribePartialResultsStability_Low = TranscribePartialResultsStability' "low"

pattern TranscribePartialResultsStability_Medium :: TranscribePartialResultsStability
pattern TranscribePartialResultsStability_Medium = TranscribePartialResultsStability' "medium"

{-# COMPLETE
  TranscribePartialResultsStability_High,
  TranscribePartialResultsStability_Low,
  TranscribePartialResultsStability_Medium,
  TranscribePartialResultsStability'
  #-}
