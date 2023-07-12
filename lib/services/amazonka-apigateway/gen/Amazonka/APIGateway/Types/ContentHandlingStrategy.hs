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
-- Module      : Amazonka.APIGateway.Types.ContentHandlingStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.ContentHandlingStrategy
  ( ContentHandlingStrategy
      ( ..,
        ContentHandlingStrategy_CONVERT_TO_BINARY,
        ContentHandlingStrategy_CONVERT_TO_TEXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentHandlingStrategy = ContentHandlingStrategy'
  { fromContentHandlingStrategy ::
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

pattern ContentHandlingStrategy_CONVERT_TO_BINARY :: ContentHandlingStrategy
pattern ContentHandlingStrategy_CONVERT_TO_BINARY = ContentHandlingStrategy' "CONVERT_TO_BINARY"

pattern ContentHandlingStrategy_CONVERT_TO_TEXT :: ContentHandlingStrategy
pattern ContentHandlingStrategy_CONVERT_TO_TEXT = ContentHandlingStrategy' "CONVERT_TO_TEXT"

{-# COMPLETE
  ContentHandlingStrategy_CONVERT_TO_BINARY,
  ContentHandlingStrategy_CONVERT_TO_TEXT,
  ContentHandlingStrategy'
  #-}
