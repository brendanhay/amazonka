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
-- Module      : Amazonka.WAFV2.Types.OversizeHandling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.OversizeHandling
  ( OversizeHandling
      ( ..,
        OversizeHandling_CONTINUE,
        OversizeHandling_MATCH,
        OversizeHandling_NO_MATCH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OversizeHandling = OversizeHandling'
  { fromOversizeHandling ::
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

pattern OversizeHandling_CONTINUE :: OversizeHandling
pattern OversizeHandling_CONTINUE = OversizeHandling' "CONTINUE"

pattern OversizeHandling_MATCH :: OversizeHandling
pattern OversizeHandling_MATCH = OversizeHandling' "MATCH"

pattern OversizeHandling_NO_MATCH :: OversizeHandling
pattern OversizeHandling_NO_MATCH = OversizeHandling' "NO_MATCH"

{-# COMPLETE
  OversizeHandling_CONTINUE,
  OversizeHandling_MATCH,
  OversizeHandling_NO_MATCH,
  OversizeHandling'
  #-}
