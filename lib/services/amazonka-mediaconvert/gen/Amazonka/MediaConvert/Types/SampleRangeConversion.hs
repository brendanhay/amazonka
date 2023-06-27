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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SampleRangeConversion
  ( SampleRangeConversion
      ( ..,
        SampleRangeConversion_LIMITED_RANGE_CLIP,
        SampleRangeConversion_LIMITED_RANGE_SQUEEZE,
        SampleRangeConversion_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how MediaConvert limits the color sample range for this output.
-- To create a limited range output from a full range input: Choose Limited
-- range squeeze. For full range inputs, MediaConvert performs a linear
-- offset to color samples equally across all pixels and frames. Color
-- samples in 10-bit outputs are limited to 64 through 940, and 8-bit
-- outputs are limited to 16 through 235. Note: For limited range inputs,
-- values for color samples are passed through to your output unchanged.
-- MediaConvert does not limit the sample range. To correct pixels in your
-- input that are out of range or out of gamut: Choose Limited range clip.
-- Use for broadcast applications. MediaConvert conforms any pixels outside
-- of the values that you specify under Minimum YUV and Maximum YUV to
-- limited range bounds. MediaConvert also corrects any YUV values that,
-- when converted to RGB, would be outside the bounds you specify under
-- Minimum RGB tolerance and Maximum RGB tolerance. With either limited
-- range conversion, MediaConvert writes the sample range metadata in the
-- output.
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

pattern SampleRangeConversion_LIMITED_RANGE_CLIP :: SampleRangeConversion
pattern SampleRangeConversion_LIMITED_RANGE_CLIP = SampleRangeConversion' "LIMITED_RANGE_CLIP"

pattern SampleRangeConversion_LIMITED_RANGE_SQUEEZE :: SampleRangeConversion
pattern SampleRangeConversion_LIMITED_RANGE_SQUEEZE = SampleRangeConversion' "LIMITED_RANGE_SQUEEZE"

pattern SampleRangeConversion_NONE :: SampleRangeConversion
pattern SampleRangeConversion_NONE = SampleRangeConversion' "NONE"

{-# COMPLETE
  SampleRangeConversion_LIMITED_RANGE_CLIP,
  SampleRangeConversion_LIMITED_RANGE_SQUEEZE,
  SampleRangeConversion_NONE,
  SampleRangeConversion'
  #-}
