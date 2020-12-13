{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeResourceType
  ( HandshakeResourceType
      ( HandshakeResourceType',
        Account,
        Organization,
        OrganizationFeatureSet,
        Email,
        MasterEmail,
        MasterName,
        Notes,
        ParentHandshake
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HandshakeResourceType = HandshakeResourceType' Lude.Text
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

pattern Account :: HandshakeResourceType
pattern Account = HandshakeResourceType' "ACCOUNT"

pattern Organization :: HandshakeResourceType
pattern Organization = HandshakeResourceType' "ORGANIZATION"

pattern OrganizationFeatureSet :: HandshakeResourceType
pattern OrganizationFeatureSet = HandshakeResourceType' "ORGANIZATION_FEATURE_SET"

pattern Email :: HandshakeResourceType
pattern Email = HandshakeResourceType' "EMAIL"

pattern MasterEmail :: HandshakeResourceType
pattern MasterEmail = HandshakeResourceType' "MASTER_EMAIL"

pattern MasterName :: HandshakeResourceType
pattern MasterName = HandshakeResourceType' "MASTER_NAME"

pattern Notes :: HandshakeResourceType
pattern Notes = HandshakeResourceType' "NOTES"

pattern ParentHandshake :: HandshakeResourceType
pattern ParentHandshake = HandshakeResourceType' "PARENT_HANDSHAKE"

{-# COMPLETE
  Account,
  Organization,
  OrganizationFeatureSet,
  Email,
  MasterEmail,
  MasterName,
  Notes,
  ParentHandshake,
  HandshakeResourceType'
  #-}
