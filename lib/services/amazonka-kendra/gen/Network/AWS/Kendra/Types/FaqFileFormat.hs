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
-- Module      : Network.AWS.Kendra.Types.FaqFileFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.FaqFileFormat
  ( FaqFileFormat
      ( ..,
        FaqFileFormat_CSV,
        FaqFileFormat_CSV_WITH_HEADER,
        FaqFileFormat_JSON
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FaqFileFormat = FaqFileFormat'
  { fromFaqFileFormat ::
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

pattern FaqFileFormat_CSV :: FaqFileFormat
pattern FaqFileFormat_CSV = FaqFileFormat' "CSV"

pattern FaqFileFormat_CSV_WITH_HEADER :: FaqFileFormat
pattern FaqFileFormat_CSV_WITH_HEADER = FaqFileFormat' "CSV_WITH_HEADER"

pattern FaqFileFormat_JSON :: FaqFileFormat
pattern FaqFileFormat_JSON = FaqFileFormat' "JSON"

{-# COMPLETE
  FaqFileFormat_CSV,
  FaqFileFormat_CSV_WITH_HEADER,
  FaqFileFormat_JSON,
  FaqFileFormat'
  #-}
