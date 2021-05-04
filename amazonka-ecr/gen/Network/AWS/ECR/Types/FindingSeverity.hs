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

import qualified Network.AWS.Prelude as Prelude

newtype FindingSeverity = FindingSeverity'
  { fromFindingSeverity ::
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
