{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Permission
  ( Permission
      ( Permission',
        FullControl,
        Write,
        WriteAcp,
        Read,
        ReadAcp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Permission = Permission' Lude.Text
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

pattern FullControl :: Permission
pattern FullControl = Permission' "FULL_CONTROL"

pattern Write :: Permission
pattern Write = Permission' "WRITE"

pattern WriteAcp :: Permission
pattern WriteAcp = Permission' "WRITE_ACP"

pattern Read :: Permission
pattern Read = Permission' "READ"

pattern ReadAcp :: Permission
pattern ReadAcp = Permission' "READ_ACP"

{-# COMPLETE
  FullControl,
  Write,
  WriteAcp,
  Read,
  ReadAcp,
  Permission'
  #-}
