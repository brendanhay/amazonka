{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Permission
  ( Permission
      ( Permission',
        PFullControl,
        PWrite,
        PWriteAcp,
        PRead,
        PReadAcp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

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

pattern PFullControl :: Permission
pattern PFullControl = Permission' "FULL_CONTROL"

pattern PWrite :: Permission
pattern PWrite = Permission' "WRITE"

pattern PWriteAcp :: Permission
pattern PWriteAcp = Permission' "WRITE_ACP"

pattern PRead :: Permission
pattern PRead = Permission' "READ"

pattern PReadAcp :: Permission
pattern PReadAcp = Permission' "READ_ACP"

{-# COMPLETE
  PFullControl,
  PWrite,
  PWriteAcp,
  PRead,
  PReadAcp,
  Permission'
  #-}
