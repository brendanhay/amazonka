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
-- Module      : Amazonka.Inspector.Types.Severity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.Severity
  ( Severity
      ( ..,
        Severity_High,
        Severity_Informational,
        Severity_Low,
        Severity_Medium,
        Severity_Undefined
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

pattern Severity_High :: Severity
pattern Severity_High = Severity' "High"

pattern Severity_Informational :: Severity
pattern Severity_Informational = Severity' "Informational"

pattern Severity_Low :: Severity
pattern Severity_Low = Severity' "Low"

pattern Severity_Medium :: Severity
pattern Severity_Medium = Severity' "Medium"

pattern Severity_Undefined :: Severity
pattern Severity_Undefined = Severity' "Undefined"

{-# COMPLETE
  Severity_High,
  Severity_Informational,
  Severity_Low,
  Severity_Medium,
  Severity_Undefined,
  Severity'
  #-}
