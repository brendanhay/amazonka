-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethodStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethodStatus
  ( ContactMethodStatus
      ( ContactMethodStatus',
        Invalid,
        PendingVerification,
        Valid
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContactMethodStatus = ContactMethodStatus' Lude.Text
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

pattern Invalid :: ContactMethodStatus
pattern Invalid = ContactMethodStatus' "Invalid"

pattern PendingVerification :: ContactMethodStatus
pattern PendingVerification = ContactMethodStatus' "PendingVerification"

pattern Valid :: ContactMethodStatus
pattern Valid = ContactMethodStatus' "Valid"

{-# COMPLETE
  Invalid,
  PendingVerification,
  Valid,
  ContactMethodStatus'
  #-}
