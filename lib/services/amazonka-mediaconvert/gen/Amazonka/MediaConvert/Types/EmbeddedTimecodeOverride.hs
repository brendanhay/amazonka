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
-- Module      : Amazonka.MediaConvert.Types.EmbeddedTimecodeOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.EmbeddedTimecodeOverride
  ( EmbeddedTimecodeOverride
      ( ..,
        EmbeddedTimecodeOverride_NONE,
        EmbeddedTimecodeOverride_USE_MDPM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Set Embedded timecode override (embeddedTimecodeOverride) to Use MDPM
-- (USE_MDPM) when your AVCHD input contains timecode tag data in the
-- Modified Digital Video Pack Metadata (MDPM). When you do, we recommend
-- you also set Timecode source (inputTimecodeSource) to Embedded
-- (EMBEDDED). Leave Embedded timecode override blank, or set to None
-- (NONE), when your input does not contain MDPM timecode.
newtype EmbeddedTimecodeOverride = EmbeddedTimecodeOverride'
  { fromEmbeddedTimecodeOverride ::
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

pattern EmbeddedTimecodeOverride_NONE :: EmbeddedTimecodeOverride
pattern EmbeddedTimecodeOverride_NONE = EmbeddedTimecodeOverride' "NONE"

pattern EmbeddedTimecodeOverride_USE_MDPM :: EmbeddedTimecodeOverride
pattern EmbeddedTimecodeOverride_USE_MDPM = EmbeddedTimecodeOverride' "USE_MDPM"

{-# COMPLETE
  EmbeddedTimecodeOverride_NONE,
  EmbeddedTimecodeOverride_USE_MDPM,
  EmbeddedTimecodeOverride'
  #-}
