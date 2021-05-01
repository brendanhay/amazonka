{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNodeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNodeType
  ( OrganizationNodeType
      ( ..,
        OrganizationNodeType_ACCOUNT,
        OrganizationNodeType_ORGANIZATION,
        OrganizationNodeType_ORGANIZATIONAL_UNIT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OrganizationNodeType = OrganizationNodeType'
  { fromOrganizationNodeType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern OrganizationNodeType_ACCOUNT :: OrganizationNodeType
pattern OrganizationNodeType_ACCOUNT = OrganizationNodeType' "ACCOUNT"

pattern OrganizationNodeType_ORGANIZATION :: OrganizationNodeType
pattern OrganizationNodeType_ORGANIZATION = OrganizationNodeType' "ORGANIZATION"

pattern OrganizationNodeType_ORGANIZATIONAL_UNIT :: OrganizationNodeType
pattern OrganizationNodeType_ORGANIZATIONAL_UNIT = OrganizationNodeType' "ORGANIZATIONAL_UNIT"

{-# COMPLETE
  OrganizationNodeType_ACCOUNT,
  OrganizationNodeType_ORGANIZATION,
  OrganizationNodeType_ORGANIZATIONAL_UNIT,
  OrganizationNodeType'
  #-}
