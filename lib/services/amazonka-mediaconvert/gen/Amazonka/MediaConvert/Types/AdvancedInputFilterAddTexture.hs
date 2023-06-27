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
-- Module      : Amazonka.MediaConvert.Types.AdvancedInputFilterAddTexture
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AdvancedInputFilterAddTexture
  ( AdvancedInputFilterAddTexture
      ( ..,
        AdvancedInputFilterAddTexture_DISABLED,
        AdvancedInputFilterAddTexture_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Add texture and detail to areas of your input video content that were
-- lost after applying the Advanced input filter. To adaptively add texture
-- and reduce softness: Choose Enabled. To not add any texture: Keep the
-- default value, Disabled. We recommend that you choose Disabled for input
-- video content that doesn\'t have texture, including screen recordings,
-- computer graphics, or cartoons.
newtype AdvancedInputFilterAddTexture = AdvancedInputFilterAddTexture'
  { fromAdvancedInputFilterAddTexture ::
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

pattern AdvancedInputFilterAddTexture_DISABLED :: AdvancedInputFilterAddTexture
pattern AdvancedInputFilterAddTexture_DISABLED = AdvancedInputFilterAddTexture' "DISABLED"

pattern AdvancedInputFilterAddTexture_ENABLED :: AdvancedInputFilterAddTexture
pattern AdvancedInputFilterAddTexture_ENABLED = AdvancedInputFilterAddTexture' "ENABLED"

{-# COMPLETE
  AdvancedInputFilterAddTexture_DISABLED,
  AdvancedInputFilterAddTexture_ENABLED,
  AdvancedInputFilterAddTexture'
  #-}
