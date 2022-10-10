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
-- Module      : Amazonka.MediaConvert.Types.F4vMoovPlacement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.F4vMoovPlacement
  ( F4vMoovPlacement
      ( ..,
        F4vMoovPlacement_NORMAL,
        F4vMoovPlacement_PROGRESSIVE_DOWNLOAD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement'
  { fromF4vMoovPlacement ::
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

pattern F4vMoovPlacement_NORMAL :: F4vMoovPlacement
pattern F4vMoovPlacement_NORMAL = F4vMoovPlacement' "NORMAL"

pattern F4vMoovPlacement_PROGRESSIVE_DOWNLOAD :: F4vMoovPlacement
pattern F4vMoovPlacement_PROGRESSIVE_DOWNLOAD = F4vMoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  F4vMoovPlacement_NORMAL,
  F4vMoovPlacement_PROGRESSIVE_DOWNLOAD,
  F4vMoovPlacement'
  #-}
