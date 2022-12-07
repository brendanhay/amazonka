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
-- Module      : Amazonka.KinesisVideoMedia.Types.StartSelectorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoMedia.Types.StartSelectorType
  ( StartSelectorType
      ( ..,
        StartSelectorType_CONTINUATION_TOKEN,
        StartSelectorType_EARLIEST,
        StartSelectorType_FRAGMENT_NUMBER,
        StartSelectorType_NOW,
        StartSelectorType_PRODUCER_TIMESTAMP,
        StartSelectorType_SERVER_TIMESTAMP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StartSelectorType = StartSelectorType'
  { fromStartSelectorType ::
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

pattern StartSelectorType_CONTINUATION_TOKEN :: StartSelectorType
pattern StartSelectorType_CONTINUATION_TOKEN = StartSelectorType' "CONTINUATION_TOKEN"

pattern StartSelectorType_EARLIEST :: StartSelectorType
pattern StartSelectorType_EARLIEST = StartSelectorType' "EARLIEST"

pattern StartSelectorType_FRAGMENT_NUMBER :: StartSelectorType
pattern StartSelectorType_FRAGMENT_NUMBER = StartSelectorType' "FRAGMENT_NUMBER"

pattern StartSelectorType_NOW :: StartSelectorType
pattern StartSelectorType_NOW = StartSelectorType' "NOW"

pattern StartSelectorType_PRODUCER_TIMESTAMP :: StartSelectorType
pattern StartSelectorType_PRODUCER_TIMESTAMP = StartSelectorType' "PRODUCER_TIMESTAMP"

pattern StartSelectorType_SERVER_TIMESTAMP :: StartSelectorType
pattern StartSelectorType_SERVER_TIMESTAMP = StartSelectorType' "SERVER_TIMESTAMP"

{-# COMPLETE
  StartSelectorType_CONTINUATION_TOKEN,
  StartSelectorType_EARLIEST,
  StartSelectorType_FRAGMENT_NUMBER,
  StartSelectorType_NOW,
  StartSelectorType_PRODUCER_TIMESTAMP,
  StartSelectorType_SERVER_TIMESTAMP,
  StartSelectorType'
  #-}
