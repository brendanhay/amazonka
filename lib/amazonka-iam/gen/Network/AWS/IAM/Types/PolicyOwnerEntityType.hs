-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyOwnerEntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyOwnerEntityType
  ( PolicyOwnerEntityType
      ( PolicyOwnerEntityType',
        POETGroup,
        POETRole,
        POETUser
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PolicyOwnerEntityType = PolicyOwnerEntityType' Lude.Text
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

pattern POETGroup :: PolicyOwnerEntityType
pattern POETGroup = PolicyOwnerEntityType' "GROUP"

pattern POETRole :: PolicyOwnerEntityType
pattern POETRole = PolicyOwnerEntityType' "ROLE"

pattern POETUser :: PolicyOwnerEntityType
pattern POETUser = PolicyOwnerEntityType' "USER"

{-# COMPLETE
  POETGroup,
  POETRole,
  POETUser,
  PolicyOwnerEntityType'
  #-}
