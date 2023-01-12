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
-- Module      : Amazonka.ECR.Types.FindingSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.FindingSeverity
  ( FindingSeverity
      ( ..,
        FindingSeverity_CRITICAL,
        FindingSeverity_HIGH,
        FindingSeverity_INFORMATIONAL,
        FindingSeverity_LOW,
        FindingSeverity_MEDIUM,
        FindingSeverity_UNDEFINED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingSeverity = FindingSeverity'
  { fromFindingSeverity ::
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

pattern FindingSeverity_CRITICAL :: FindingSeverity
pattern FindingSeverity_CRITICAL = FindingSeverity' "CRITICAL"

pattern FindingSeverity_HIGH :: FindingSeverity
pattern FindingSeverity_HIGH = FindingSeverity' "HIGH"

pattern FindingSeverity_INFORMATIONAL :: FindingSeverity
pattern FindingSeverity_INFORMATIONAL = FindingSeverity' "INFORMATIONAL"

pattern FindingSeverity_LOW :: FindingSeverity
pattern FindingSeverity_LOW = FindingSeverity' "LOW"

pattern FindingSeverity_MEDIUM :: FindingSeverity
pattern FindingSeverity_MEDIUM = FindingSeverity' "MEDIUM"

pattern FindingSeverity_UNDEFINED :: FindingSeverity
pattern FindingSeverity_UNDEFINED = FindingSeverity' "UNDEFINED"

{-# COMPLETE
  FindingSeverity_CRITICAL,
  FindingSeverity_HIGH,
  FindingSeverity_INFORMATIONAL,
  FindingSeverity_LOW,
  FindingSeverity_MEDIUM,
  FindingSeverity_UNDEFINED,
  FindingSeverity'
  #-}
