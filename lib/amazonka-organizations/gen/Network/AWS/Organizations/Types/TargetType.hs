{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.TargetType
  ( TargetType
      ( TargetType',
        TTAccount,
        TTOrganizationalUnit,
        TTRoot
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetType = TargetType' Lude.Text
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

pattern TTAccount :: TargetType
pattern TTAccount = TargetType' "ACCOUNT"

pattern TTOrganizationalUnit :: TargetType
pattern TTOrganizationalUnit = TargetType' "ORGANIZATIONAL_UNIT"

pattern TTRoot :: TargetType
pattern TTRoot = TargetType' "ROOT"

{-# COMPLETE
  TTAccount,
  TTOrganizationalUnit,
  TTRoot,
  TargetType'
  #-}
