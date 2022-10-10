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
-- Module      : Amazonka.LookoutMetrics.Types.SnsFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.SnsFormat
  ( SnsFormat
      ( ..,
        SnsFormat_JSON,
        SnsFormat_LONG_TEXT,
        SnsFormat_SHORT_TEXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SnsFormat = SnsFormat'
  { fromSnsFormat ::
      Core.Text
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

pattern SnsFormat_JSON :: SnsFormat
pattern SnsFormat_JSON = SnsFormat' "JSON"

pattern SnsFormat_LONG_TEXT :: SnsFormat
pattern SnsFormat_LONG_TEXT = SnsFormat' "LONG_TEXT"

pattern SnsFormat_SHORT_TEXT :: SnsFormat
pattern SnsFormat_SHORT_TEXT = SnsFormat' "SHORT_TEXT"

{-# COMPLETE
  SnsFormat_JSON,
  SnsFormat_LONG_TEXT,
  SnsFormat_SHORT_TEXT,
  SnsFormat'
  #-}
