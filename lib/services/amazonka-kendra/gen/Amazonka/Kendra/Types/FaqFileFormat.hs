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
-- Module      : Amazonka.Kendra.Types.FaqFileFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FaqFileFormat
  ( FaqFileFormat
      ( ..,
        FaqFileFormat_CSV,
        FaqFileFormat_CSV_WITH_HEADER,
        FaqFileFormat_JSON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FaqFileFormat = FaqFileFormat'
  { fromFaqFileFormat ::
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
