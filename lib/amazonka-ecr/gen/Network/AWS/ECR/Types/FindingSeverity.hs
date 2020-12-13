{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Informational,
        Low,
        Medium,
        High,
        Critical,
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

pattern Informational :: FindingSeverity
pattern Informational = FindingSeverity' "INFORMATIONAL"

pattern Low :: FindingSeverity
pattern Low = FindingSeverity' "LOW"

pattern Medium :: FindingSeverity
pattern Medium = FindingSeverity' "MEDIUM"

pattern High :: FindingSeverity
pattern High = FindingSeverity' "HIGH"

pattern Critical :: FindingSeverity
pattern Critical = FindingSeverity' "CRITICAL"

pattern Undefined :: FindingSeverity
pattern Undefined = FindingSeverity' "UNDEFINED"

{-# COMPLETE
  Informational,
  Low,
  Medium,
  High,
  Critical,
  Undefined,
  FindingSeverity'
  #-}
