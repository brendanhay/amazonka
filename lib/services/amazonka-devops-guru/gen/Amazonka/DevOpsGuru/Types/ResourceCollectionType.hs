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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceCollectionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceCollectionType
  ( ResourceCollectionType
      ( ..,
        ResourceCollectionType_AWS_CLOUD_FORMATION,
        ResourceCollectionType_AWS_SERVICE,
        ResourceCollectionType_AWS_TAGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceCollectionType = ResourceCollectionType'
  { fromResourceCollectionType ::
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

pattern ResourceCollectionType_AWS_CLOUD_FORMATION :: ResourceCollectionType
pattern ResourceCollectionType_AWS_CLOUD_FORMATION = ResourceCollectionType' "AWS_CLOUD_FORMATION"

pattern ResourceCollectionType_AWS_SERVICE :: ResourceCollectionType
pattern ResourceCollectionType_AWS_SERVICE = ResourceCollectionType' "AWS_SERVICE"

pattern ResourceCollectionType_AWS_TAGS :: ResourceCollectionType
pattern ResourceCollectionType_AWS_TAGS = ResourceCollectionType' "AWS_TAGS"

{-# COMPLETE
  ResourceCollectionType_AWS_CLOUD_FORMATION,
  ResourceCollectionType_AWS_SERVICE,
  ResourceCollectionType_AWS_TAGS,
  ResourceCollectionType'
  #-}
