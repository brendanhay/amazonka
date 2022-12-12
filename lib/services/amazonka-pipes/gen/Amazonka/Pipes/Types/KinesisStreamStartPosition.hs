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
-- Module      : Amazonka.Pipes.Types.KinesisStreamStartPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.KinesisStreamStartPosition
  ( KinesisStreamStartPosition
      ( ..,
        KinesisStreamStartPosition_AT_TIMESTAMP,
        KinesisStreamStartPosition_LATEST,
        KinesisStreamStartPosition_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KinesisStreamStartPosition = KinesisStreamStartPosition'
  { fromKinesisStreamStartPosition ::
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

pattern KinesisStreamStartPosition_AT_TIMESTAMP :: KinesisStreamStartPosition
pattern KinesisStreamStartPosition_AT_TIMESTAMP = KinesisStreamStartPosition' "AT_TIMESTAMP"

pattern KinesisStreamStartPosition_LATEST :: KinesisStreamStartPosition
pattern KinesisStreamStartPosition_LATEST = KinesisStreamStartPosition' "LATEST"

pattern KinesisStreamStartPosition_TRIM_HORIZON :: KinesisStreamStartPosition
pattern KinesisStreamStartPosition_TRIM_HORIZON = KinesisStreamStartPosition' "TRIM_HORIZON"

{-# COMPLETE
  KinesisStreamStartPosition_AT_TIMESTAMP,
  KinesisStreamStartPosition_LATEST,
  KinesisStreamStartPosition_TRIM_HORIZON,
  KinesisStreamStartPosition'
  #-}
