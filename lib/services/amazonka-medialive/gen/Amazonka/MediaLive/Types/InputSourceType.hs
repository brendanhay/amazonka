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
-- Module      : Amazonka.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSourceType
  ( InputSourceType
      ( ..,
        InputSourceType_DYNAMIC,
        InputSourceType_STATIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | There are two types of input sources, static and dynamic. If an input
-- source is dynamic you can change the source url of the input dynamically
-- using an input switch action. Currently, two input types support a
-- dynamic url at this time, MP4_FILE and TS_FILE. By default all input
-- sources are static.
newtype InputSourceType = InputSourceType'
  { fromInputSourceType ::
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

pattern InputSourceType_DYNAMIC :: InputSourceType
pattern InputSourceType_DYNAMIC = InputSourceType' "DYNAMIC"

pattern InputSourceType_STATIC :: InputSourceType
pattern InputSourceType_STATIC = InputSourceType' "STATIC"

{-# COMPLETE
  InputSourceType_DYNAMIC,
  InputSourceType_STATIC,
  InputSourceType'
  #-}
