{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Types.OrganizationNodeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.OrganizationNodeType
  ( OrganizationNodeType
      ( ..,
        OrganizationNodeType_ACCOUNT,
        OrganizationNodeType_ORGANIZATION,
        OrganizationNodeType_ORGANIZATIONAL_UNIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrganizationNodeType = OrganizationNodeType'
  { fromOrganizationNodeType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
