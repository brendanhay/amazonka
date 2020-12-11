-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.FindingSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.FindingSeverity
  ( FindingSeverity
      ( FindingSeverity',
        Critical,
        High,
        Informational,
        Low,
        Medium,
        Undefined
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FindingSeverity = FindingSeverity' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Critical :: FindingSeverity
pattern Critical = FindingSeverity' "CRITICAL"

pattern High :: FindingSeverity
pattern High = FindingSeverity' "HIGH"

pattern Informational :: FindingSeverity
pattern Informational = FindingSeverity' "INFORMATIONAL"

pattern Low :: FindingSeverity
pattern Low = FindingSeverity' "LOW"

pattern Medium :: FindingSeverity
pattern Medium = FindingSeverity' "MEDIUM"

pattern Undefined :: FindingSeverity
pattern Undefined = FindingSeverity' "UNDEFINED"

{-# COMPLETE
  Critical,
  High,
  Informational,
  Low,
  Medium,
  Undefined,
  FindingSeverity'
  #-}
