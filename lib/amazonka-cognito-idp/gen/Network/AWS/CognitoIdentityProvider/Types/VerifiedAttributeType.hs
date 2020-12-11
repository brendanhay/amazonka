-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
  ( VerifiedAttributeType
      ( VerifiedAttributeType',
        Email,
        PhoneNumber
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VerifiedAttributeType = VerifiedAttributeType' Lude.Text
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

pattern Email :: VerifiedAttributeType
pattern Email = VerifiedAttributeType' "email"

pattern PhoneNumber :: VerifiedAttributeType
pattern PhoneNumber = VerifiedAttributeType' "phone_number"

{-# COMPLETE
  Email,
  PhoneNumber,
  VerifiedAttributeType'
  #-}
