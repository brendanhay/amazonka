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
-- Module      : Amazonka.Pipes.Types.DynamoDBStreamStartPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.DynamoDBStreamStartPosition
  ( DynamoDBStreamStartPosition
      ( ..,
        DynamoDBStreamStartPosition_LATEST,
        DynamoDBStreamStartPosition_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DynamoDBStreamStartPosition = DynamoDBStreamStartPosition'
  { fromDynamoDBStreamStartPosition ::
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

pattern DynamoDBStreamStartPosition_LATEST :: DynamoDBStreamStartPosition
pattern DynamoDBStreamStartPosition_LATEST = DynamoDBStreamStartPosition' "LATEST"

pattern DynamoDBStreamStartPosition_TRIM_HORIZON :: DynamoDBStreamStartPosition
pattern DynamoDBStreamStartPosition_TRIM_HORIZON = DynamoDBStreamStartPosition' "TRIM_HORIZON"

{-# COMPLETE
  DynamoDBStreamStartPosition_LATEST,
  DynamoDBStreamStartPosition_TRIM_HORIZON,
  DynamoDBStreamStartPosition'
  #-}
