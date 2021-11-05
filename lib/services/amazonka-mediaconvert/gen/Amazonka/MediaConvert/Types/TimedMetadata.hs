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
-- Module      : Amazonka.MediaConvert.Types.TimedMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TimedMetadata
  ( TimedMetadata
      ( ..,
        TimedMetadata_NONE,
        TimedMetadata_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Applies only to HLS outputs. Use this setting to specify whether the
-- service inserts the ID3 timed metadata from the input in this output.
newtype TimedMetadata = TimedMetadata'
  { fromTimedMetadata ::
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

pattern TimedMetadata_NONE :: TimedMetadata
pattern TimedMetadata_NONE = TimedMetadata' "NONE"

pattern TimedMetadata_PASSTHROUGH :: TimedMetadata
pattern TimedMetadata_PASSTHROUGH = TimedMetadata' "PASSTHROUGH"

{-# COMPLETE
  TimedMetadata_NONE,
  TimedMetadata_PASSTHROUGH,
  TimedMetadata'
  #-}
