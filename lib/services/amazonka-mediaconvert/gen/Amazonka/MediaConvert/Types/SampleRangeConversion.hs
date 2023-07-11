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
-- Module      : Amazonka.MediaConvert.Types.SampleRangeConversion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SampleRangeConversion
  ( SampleRangeConversion
      ( ..,
        SampleRangeConversion_LIMITED_RANGE_SQUEEZE,
        SampleRangeConversion_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the video color sample range for this output. To create a full
-- range output, you must start with a full range YUV input and keep the
-- default value, None (NONE). To create a limited range output from a full
-- range input, choose Limited range (LIMITED_RANGE_SQUEEZE). With RGB
-- inputs, your output is always limited range, regardless of your choice
-- here. When you create a limited range output from a full range input,
-- MediaConvert limits the active pixel values in a way that depends on the
-- output\'s bit depth: 8-bit outputs contain only values from 16 through
-- 235 and 10-bit outputs contain only values from 64 through 940. With
-- this conversion, MediaConvert also changes the output metadata to note
-- the limited range.
newtype SampleRangeConversion = SampleRangeConversion'
  { fromSampleRangeConversion ::
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

pattern SampleRangeConversion_LIMITED_RANGE_SQUEEZE :: SampleRangeConversion
pattern SampleRangeConversion_LIMITED_RANGE_SQUEEZE = SampleRangeConversion' "LIMITED_RANGE_SQUEEZE"

pattern SampleRangeConversion_NONE :: SampleRangeConversion
pattern SampleRangeConversion_NONE = SampleRangeConversion' "NONE"

{-# COMPLETE
  SampleRangeConversion_LIMITED_RANGE_SQUEEZE,
  SampleRangeConversion_NONE,
  SampleRangeConversion'
  #-}
