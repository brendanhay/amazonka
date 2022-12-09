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
-- Module      : Amazonka.OpenSearchServerless.Types.CollectionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CollectionStatus
  ( CollectionStatus
      ( ..,
        CollectionStatus_ACTIVE,
        CollectionStatus_CREATING,
        CollectionStatus_DELETING,
        CollectionStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CollectionStatus = CollectionStatus'
  { fromCollectionStatus ::
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

pattern CollectionStatus_ACTIVE :: CollectionStatus
pattern CollectionStatus_ACTIVE = CollectionStatus' "ACTIVE"

pattern CollectionStatus_CREATING :: CollectionStatus
pattern CollectionStatus_CREATING = CollectionStatus' "CREATING"

pattern CollectionStatus_DELETING :: CollectionStatus
pattern CollectionStatus_DELETING = CollectionStatus' "DELETING"

pattern CollectionStatus_FAILED :: CollectionStatus
pattern CollectionStatus_FAILED = CollectionStatus' "FAILED"

{-# COMPLETE
  CollectionStatus_ACTIVE,
  CollectionStatus_CREATING,
  CollectionStatus_DELETING,
  CollectionStatus_FAILED,
  CollectionStatus'
  #-}
