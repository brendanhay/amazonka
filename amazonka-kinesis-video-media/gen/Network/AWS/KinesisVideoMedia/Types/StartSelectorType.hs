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
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelectorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types.StartSelectorType
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

import qualified Network.AWS.Core as Core

newtype StartSelectorType = StartSelectorType'
  { fromStartSelectorType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
