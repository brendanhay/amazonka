-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CSVHeaderOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CSVHeaderOption
  ( CSVHeaderOption
      ( CSVHeaderOption',
        Absent,
        Present,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CSVHeaderOption = CSVHeaderOption' Lude.Text
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

pattern Absent :: CSVHeaderOption
pattern Absent = CSVHeaderOption' "ABSENT"

pattern Present :: CSVHeaderOption
pattern Present = CSVHeaderOption' "PRESENT"

pattern Unknown :: CSVHeaderOption
pattern Unknown = CSVHeaderOption' "UNKNOWN"

{-# COMPLETE
  Absent,
  Present,
  Unknown,
  CSVHeaderOption'
  #-}
