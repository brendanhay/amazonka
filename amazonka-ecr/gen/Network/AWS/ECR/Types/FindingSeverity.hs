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
-- Module      : Network.AWS.ECR.Types.FindingSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.FindingSeverity
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

import qualified Network.AWS.Core as Core

newtype FindingSeverity = FindingSeverity'
  { fromFindingSeverity ::
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
