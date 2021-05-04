{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype Severity = Severity'
  { fromSeverity ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
