{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNodeType
  ( OrganizationNodeType
      ( OrganizationNodeType',
        OrganizationNodeTypeOrganization,
        OrganizationNodeTypeOrganizationalUnit,
        OrganizationNodeTypeAccount,
        fromOrganizationNodeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OrganizationNodeType = OrganizationNodeType'
  { fromOrganizationNodeType ::
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

pattern OrganizationNodeTypeOrganization :: OrganizationNodeType
pattern OrganizationNodeTypeOrganization = OrganizationNodeType' "ORGANIZATION"

pattern OrganizationNodeTypeOrganizationalUnit :: OrganizationNodeType
pattern OrganizationNodeTypeOrganizationalUnit = OrganizationNodeType' "ORGANIZATIONAL_UNIT"

pattern OrganizationNodeTypeAccount :: OrganizationNodeType
pattern OrganizationNodeTypeAccount = OrganizationNodeType' "ACCOUNT"

{-# COMPLETE
  OrganizationNodeTypeOrganization,
  OrganizationNodeTypeOrganizationalUnit,
  OrganizationNodeTypeAccount,
  OrganizationNodeType'
  #-}
