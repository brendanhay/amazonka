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
-- Module      : Amazonka.MediaConvert.Types.AacRawFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AacRawFormat
  ( AacRawFormat
      ( ..,
        AacRawFormat_LATM_LOAS,
        AacRawFormat_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enables LATM\/LOAS AAC output. Note that if you use LATM\/LOAS AAC in an
-- output, you must choose \"No container\" for the output container.
newtype AacRawFormat = AacRawFormat'
  { fromAacRawFormat ::
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

pattern AacRawFormat_LATM_LOAS :: AacRawFormat
pattern AacRawFormat_LATM_LOAS = AacRawFormat' "LATM_LOAS"

pattern AacRawFormat_NONE :: AacRawFormat
pattern AacRawFormat_NONE = AacRawFormat' "NONE"

{-# COMPLETE
  AacRawFormat_LATM_LOAS,
  AacRawFormat_NONE,
  AacRawFormat'
  #-}
