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
-- Module      : Amazonka.CodeGuruReviewer.Types.Severity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.Severity
  ( Severity
      ( ..,
        Severity_Critical,
        Severity_High,
        Severity_Info,
        Severity_Low,
        Severity_Medium
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Severity = Severity'
  { fromSeverity ::
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

pattern Severity_Critical :: Severity
pattern Severity_Critical = Severity' "Critical"

pattern Severity_High :: Severity
pattern Severity_High = Severity' "High"

pattern Severity_Info :: Severity
pattern Severity_Info = Severity' "Info"

pattern Severity_Low :: Severity
pattern Severity_Low = Severity' "Low"

pattern Severity_Medium :: Severity
pattern Severity_Medium = Severity' "Medium"

{-# COMPLETE
  Severity_Critical,
  Severity_High,
  Severity_Info,
  Severity_Low,
  Severity_Medium,
  Severity'
  #-}
