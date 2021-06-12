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
-- Module      : Network.AWS.Inspector.Types.Severity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Severity
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

import qualified Network.AWS.Core as Core

newtype Severity = Severity'
  { fromSeverity ::
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
