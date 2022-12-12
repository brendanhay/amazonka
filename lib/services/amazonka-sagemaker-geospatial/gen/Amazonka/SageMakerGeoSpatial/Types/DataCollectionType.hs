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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.DataCollectionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.DataCollectionType
  ( DataCollectionType
      ( ..,
        DataCollectionType_PREMIUM,
        DataCollectionType_PUBLIC,
        DataCollectionType_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataCollectionType = DataCollectionType'
  { fromDataCollectionType ::
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

pattern DataCollectionType_PREMIUM :: DataCollectionType
pattern DataCollectionType_PREMIUM = DataCollectionType' "PREMIUM"

pattern DataCollectionType_PUBLIC :: DataCollectionType
pattern DataCollectionType_PUBLIC = DataCollectionType' "PUBLIC"

pattern DataCollectionType_USER :: DataCollectionType
pattern DataCollectionType_USER = DataCollectionType' "USER"

{-# COMPLETE
  DataCollectionType_PREMIUM,
  DataCollectionType_PUBLIC,
  DataCollectionType_USER,
  DataCollectionType'
  #-}
