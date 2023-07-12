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
-- Module      : Amazonka.WAFV2.Types.MapMatchScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.MapMatchScope
  ( MapMatchScope
      ( ..,
        MapMatchScope_ALL,
        MapMatchScope_KEY,
        MapMatchScope_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MapMatchScope = MapMatchScope'
  { fromMapMatchScope ::
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

pattern MapMatchScope_ALL :: MapMatchScope
pattern MapMatchScope_ALL = MapMatchScope' "ALL"

pattern MapMatchScope_KEY :: MapMatchScope
pattern MapMatchScope_KEY = MapMatchScope' "KEY"

pattern MapMatchScope_VALUE :: MapMatchScope
pattern MapMatchScope_VALUE = MapMatchScope' "VALUE"

{-# COMPLETE
  MapMatchScope_ALL,
  MapMatchScope_KEY,
  MapMatchScope_VALUE,
  MapMatchScope'
  #-}
