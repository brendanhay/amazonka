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
        HandshakeResourceTypeAccount,
        HandshakeResourceTypeOrganization,
        HandshakeResourceTypeOrganizationFeatureSet,
        HandshakeResourceTypeEmail,
        HandshakeResourceTypeMasterEmail,
        HandshakeResourceTypeMasterName,
        HandshakeResourceTypeNotes,
        HandshakeResourceTypeParentHandshake,
        fromHandshakeResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HandshakeResourceType = HandshakeResourceType'
  { fromHandshakeResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HandshakeResourceTypeAccount :: HandshakeResourceType
pattern HandshakeResourceTypeAccount = HandshakeResourceType' "ACCOUNT"

pattern HandshakeResourceTypeOrganization :: HandshakeResourceType
pattern HandshakeResourceTypeOrganization = HandshakeResourceType' "ORGANIZATION"

pattern HandshakeResourceTypeOrganizationFeatureSet :: HandshakeResourceType
pattern HandshakeResourceTypeOrganizationFeatureSet = HandshakeResourceType' "ORGANIZATION_FEATURE_SET"

pattern HandshakeResourceTypeEmail :: HandshakeResourceType
pattern HandshakeResourceTypeEmail = HandshakeResourceType' "EMAIL"

pattern HandshakeResourceTypeMasterEmail :: HandshakeResourceType
pattern HandshakeResourceTypeMasterEmail = HandshakeResourceType' "MASTER_EMAIL"

pattern HandshakeResourceTypeMasterName :: HandshakeResourceType
pattern HandshakeResourceTypeMasterName = HandshakeResourceType' "MASTER_NAME"

pattern HandshakeResourceTypeNotes :: HandshakeResourceType
pattern HandshakeResourceTypeNotes = HandshakeResourceType' "NOTES"

pattern HandshakeResourceTypeParentHandshake :: HandshakeResourceType
pattern HandshakeResourceTypeParentHandshake = HandshakeResourceType' "PARENT_HANDSHAKE"

{-# COMPLETE
  HandshakeResourceTypeAccount,
  HandshakeResourceTypeOrganization,
  HandshakeResourceTypeOrganizationFeatureSet,
  HandshakeResourceTypeEmail,
  HandshakeResourceTypeMasterEmail,
  HandshakeResourceTypeMasterName,
  HandshakeResourceTypeNotes,
  HandshakeResourceTypeParentHandshake,
  HandshakeResourceType'
  #-}
