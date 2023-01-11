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
-- Module      : Amazonka.MacieV2.Types.DataIdentifierType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DataIdentifierType
  ( DataIdentifierType
      ( ..,
        DataIdentifierType_CUSTOM,
        DataIdentifierType_MANAGED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of data identifier that detected a specific type of sensitive
-- data in an S3 bucket. Possible values are:
newtype DataIdentifierType = DataIdentifierType'
  { fromDataIdentifierType ::
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

pattern DataIdentifierType_CUSTOM :: DataIdentifierType
pattern DataIdentifierType_CUSTOM = DataIdentifierType' "CUSTOM"

pattern DataIdentifierType_MANAGED :: DataIdentifierType
pattern DataIdentifierType_MANAGED = DataIdentifierType' "MANAGED"

{-# COMPLETE
  DataIdentifierType_CUSTOM,
  DataIdentifierType_MANAGED,
  DataIdentifierType'
  #-}
