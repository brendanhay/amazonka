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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OpsItemRelatedItemsFilterKey = OpsItemRelatedItemsFilterKey'
  { fromOpsItemRelatedItemsFilterKey ::
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
