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
-- Module      : Amazonka.KinesisVideo.Types.StrategyOnFullSize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.StrategyOnFullSize
  ( StrategyOnFullSize
      ( ..,
        StrategyOnFullSize_DELETE_OLDEST_MEDIA,
        StrategyOnFullSize_DENY_NEW_MEDIA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StrategyOnFullSize = StrategyOnFullSize'
  { fromStrategyOnFullSize ::
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

pattern StrategyOnFullSize_DELETE_OLDEST_MEDIA :: StrategyOnFullSize
pattern StrategyOnFullSize_DELETE_OLDEST_MEDIA = StrategyOnFullSize' "DELETE_OLDEST_MEDIA"

pattern StrategyOnFullSize_DENY_NEW_MEDIA :: StrategyOnFullSize
pattern StrategyOnFullSize_DENY_NEW_MEDIA = StrategyOnFullSize' "DENY_NEW_MEDIA"

{-# COMPLETE
  StrategyOnFullSize_DELETE_OLDEST_MEDIA,
  StrategyOnFullSize_DENY_NEW_MEDIA,
  StrategyOnFullSize'
  #-}
