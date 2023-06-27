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
-- Module      : Amazonka.MediaConvert.Types.AdvancedInputFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AdvancedInputFilter
  ( AdvancedInputFilter
      ( ..,
        AdvancedInputFilter_DISABLED,
        AdvancedInputFilter_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use to remove noise, blocking, blurriness, or ringing from your input as
-- a pre-filter step before encoding. The Advanced input filter removes
-- more types of compression artifacts and is an improvement when compared
-- to basic Deblock and Denoise filters. To remove video compression
-- artifacts from your input and improve the video quality: Choose Enabled.
-- Additionally, this filter can help increase the video quality of your
-- output relative to its bitrate, since noisy inputs are more complex and
-- require more bits to encode. To help restore loss of detail after
-- applying the filter, you can optionally add texture or sharpening as an
-- additional step. Jobs that use this feature incur pro-tier pricing. To
-- not apply advanced input filtering: Choose Disabled. Note that you can
-- still apply basic filtering with Deblock and Denoise.
newtype AdvancedInputFilter = AdvancedInputFilter'
  { fromAdvancedInputFilter ::
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

pattern AdvancedInputFilter_DISABLED :: AdvancedInputFilter
pattern AdvancedInputFilter_DISABLED = AdvancedInputFilter' "DISABLED"

pattern AdvancedInputFilter_ENABLED :: AdvancedInputFilter
pattern AdvancedInputFilter_ENABLED = AdvancedInputFilter' "ENABLED"

{-# COMPLETE
  AdvancedInputFilter_DISABLED,
  AdvancedInputFilter_ENABLED,
  AdvancedInputFilter'
  #-}
