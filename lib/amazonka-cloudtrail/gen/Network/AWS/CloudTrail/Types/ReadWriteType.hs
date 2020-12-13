{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ReadWriteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ReadWriteType
  ( ReadWriteType
      ( ReadWriteType',
        RWTReadOnly,
        RWTWriteOnly,
        RWTAll
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReadWriteType = ReadWriteType' Lude.Text
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

pattern RWTReadOnly :: ReadWriteType
pattern RWTReadOnly = ReadWriteType' "ReadOnly"

pattern RWTWriteOnly :: ReadWriteType
pattern RWTWriteOnly = ReadWriteType' "WriteOnly"

pattern RWTAll :: ReadWriteType
pattern RWTAll = ReadWriteType' "All"

{-# COMPLETE
  RWTReadOnly,
  RWTWriteOnly,
  RWTAll,
  ReadWriteType'
  #-}
