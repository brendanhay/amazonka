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
-- Module      : Amazonka.DevOpsGuru.Types.OrganizationResourceCollectionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.OrganizationResourceCollectionType
  ( OrganizationResourceCollectionType
      ( ..,
        OrganizationResourceCollectionType_AWS_ACCOUNT,
        OrganizationResourceCollectionType_AWS_CLOUD_FORMATION,
        OrganizationResourceCollectionType_AWS_SERVICE,
        OrganizationResourceCollectionType_AWS_TAGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OrganizationResourceCollectionType = OrganizationResourceCollectionType'
  { fromOrganizationResourceCollectionType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern OrganizationResourceCollectionType_AWS_ACCOUNT :: OrganizationResourceCollectionType
pattern OrganizationResourceCollectionType_AWS_ACCOUNT = OrganizationResourceCollectionType' "AWS_ACCOUNT"

pattern OrganizationResourceCollectionType_AWS_CLOUD_FORMATION :: OrganizationResourceCollectionType
pattern OrganizationResourceCollectionType_AWS_CLOUD_FORMATION = OrganizationResourceCollectionType' "AWS_CLOUD_FORMATION"

pattern OrganizationResourceCollectionType_AWS_SERVICE :: OrganizationResourceCollectionType
pattern OrganizationResourceCollectionType_AWS_SERVICE = OrganizationResourceCollectionType' "AWS_SERVICE"

pattern OrganizationResourceCollectionType_AWS_TAGS :: OrganizationResourceCollectionType
pattern OrganizationResourceCollectionType_AWS_TAGS = OrganizationResourceCollectionType' "AWS_TAGS"

{-# COMPLETE
  OrganizationResourceCollectionType_AWS_ACCOUNT,
  OrganizationResourceCollectionType_AWS_CLOUD_FORMATION,
  OrganizationResourceCollectionType_AWS_SERVICE,
  OrganizationResourceCollectionType_AWS_TAGS,
  OrganizationResourceCollectionType'
  #-}
