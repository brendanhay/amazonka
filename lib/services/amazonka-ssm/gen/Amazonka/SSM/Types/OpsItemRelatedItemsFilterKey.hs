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
-- Module      : Amazonka.SSM.Types.OpsItemRelatedItemsFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemRelatedItemsFilterKey
  ( OpsItemRelatedItemsFilterKey
      ( ..,
        OpsItemRelatedItemsFilterKey_AssociationId,
        OpsItemRelatedItemsFilterKey_ResourceType,
        OpsItemRelatedItemsFilterKey_ResourceUri
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OpsItemRelatedItemsFilterKey = OpsItemRelatedItemsFilterKey'
  { fromOpsItemRelatedItemsFilterKey ::
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

pattern OpsItemRelatedItemsFilterKey_AssociationId :: OpsItemRelatedItemsFilterKey
pattern OpsItemRelatedItemsFilterKey_AssociationId = OpsItemRelatedItemsFilterKey' "AssociationId"

pattern OpsItemRelatedItemsFilterKey_ResourceType :: OpsItemRelatedItemsFilterKey
pattern OpsItemRelatedItemsFilterKey_ResourceType = OpsItemRelatedItemsFilterKey' "ResourceType"

pattern OpsItemRelatedItemsFilterKey_ResourceUri :: OpsItemRelatedItemsFilterKey
pattern OpsItemRelatedItemsFilterKey_ResourceUri = OpsItemRelatedItemsFilterKey' "ResourceUri"

{-# COMPLETE
  OpsItemRelatedItemsFilterKey_AssociationId,
  OpsItemRelatedItemsFilterKey_ResourceType,
  OpsItemRelatedItemsFilterKey_ResourceUri,
  OpsItemRelatedItemsFilterKey'
  #-}
