{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrincipalType
  ( PrincipalType
      ( PrincipalType',
        PTAccount,
        PTAll,
        PTOrganizationUnit,
        PTRole,
        PTService,
        PTUser
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PrincipalType = PrincipalType' Lude.Text
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

pattern PTAccount :: PrincipalType
pattern PTAccount = PrincipalType' "Account"

pattern PTAll :: PrincipalType
pattern PTAll = PrincipalType' "All"

pattern PTOrganizationUnit :: PrincipalType
pattern PTOrganizationUnit = PrincipalType' "OrganizationUnit"

pattern PTRole :: PrincipalType
pattern PTRole = PrincipalType' "Role"

pattern PTService :: PrincipalType
pattern PTService = PrincipalType' "Service"

pattern PTUser :: PrincipalType
pattern PTUser = PrincipalType' "User"

{-# COMPLETE
  PTAccount,
  PTAll,
  PTOrganizationUnit,
  PTRole,
  PTService,
  PTUser,
  PrincipalType'
  #-}
