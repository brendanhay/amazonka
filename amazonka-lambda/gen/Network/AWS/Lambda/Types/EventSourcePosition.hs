{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourcePosition
  ( EventSourcePosition
      ( ..,
        EventSourcePosition_AT_TIMESTAMP,
        EventSourcePosition_LATEST,
        EventSourcePosition_TRIM_HORIZON
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EventSourcePosition = EventSourcePosition'
  { fromEventSourcePosition ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern EventSourcePosition_AT_TIMESTAMP :: EventSourcePosition
pattern EventSourcePosition_AT_TIMESTAMP = EventSourcePosition' "AT_TIMESTAMP"

pattern EventSourcePosition_LATEST :: EventSourcePosition
pattern EventSourcePosition_LATEST = EventSourcePosition' "LATEST"

pattern EventSourcePosition_TRIM_HORIZON :: EventSourcePosition
pattern EventSourcePosition_TRIM_HORIZON = EventSourcePosition' "TRIM_HORIZON"

{-# COMPLETE
  EventSourcePosition_AT_TIMESTAMP,
  EventSourcePosition_LATEST,
  EventSourcePosition_TRIM_HORIZON,
  EventSourcePosition'
  #-}
