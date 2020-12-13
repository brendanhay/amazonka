{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Severity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Severity
  ( Severity
      ( Severity',
        Low,
        Medium,
        High,
        Informational,
        Undefined
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Severity = Severity' Lude.Text
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

pattern Low :: Severity
pattern Low = Severity' "Low"

pattern Medium :: Severity
pattern Medium = Severity' "Medium"

pattern High :: Severity
pattern High = Severity' "High"

pattern Informational :: Severity
pattern Informational = Severity' "Informational"

pattern Undefined :: Severity
pattern Undefined = Severity' "Undefined"

{-# COMPLETE
  Low,
  Medium,
  High,
  Informational,
  Undefined,
  Severity'
  #-}
