-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.PermissionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.PermissionType
  ( PermissionType
      ( PermissionType',
        FullAccess,
        SendAs,
        SendOnBehalf
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PermissionType = PermissionType' Lude.Text
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

pattern FullAccess :: PermissionType
pattern FullAccess = PermissionType' "FULL_ACCESS"

pattern SendAs :: PermissionType
pattern SendAs = PermissionType' "SEND_AS"

pattern SendOnBehalf :: PermissionType
pattern SendOnBehalf = PermissionType' "SEND_ON_BEHALF"

{-# COMPLETE
  FullAccess,
  SendAs,
  SendOnBehalf,
  PermissionType'
  #-}
