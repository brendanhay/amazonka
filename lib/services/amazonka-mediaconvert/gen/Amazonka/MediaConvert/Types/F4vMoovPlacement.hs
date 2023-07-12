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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement'
  { fromF4vMoovPlacement ::
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

pattern F4vMoovPlacement_NORMAL :: F4vMoovPlacement
pattern F4vMoovPlacement_NORMAL = F4vMoovPlacement' "NORMAL"

pattern F4vMoovPlacement_PROGRESSIVE_DOWNLOAD :: F4vMoovPlacement
pattern F4vMoovPlacement_PROGRESSIVE_DOWNLOAD = F4vMoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  F4vMoovPlacement_NORMAL,
  F4vMoovPlacement_PROGRESSIVE_DOWNLOAD,
  F4vMoovPlacement'
  #-}
