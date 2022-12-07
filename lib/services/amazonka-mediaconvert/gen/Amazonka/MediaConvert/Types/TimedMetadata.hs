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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH) to include
-- ID3 metadata in this output. This includes ID3 metadata from the
-- following features: ID3 timestamp period (timedMetadataId3Period), and
-- Custom ID3 metadata inserter (timedMetadataInsertion). To exclude this
-- ID3 metadata in this output: set ID3 metadata to None (NONE) or leave
-- blank.
newtype TimedMetadata = TimedMetadata'
  { fromTimedMetadata ::
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

pattern TimedMetadata_NONE :: TimedMetadata
pattern TimedMetadata_NONE = TimedMetadata' "NONE"

pattern TimedMetadata_PASSTHROUGH :: TimedMetadata
pattern TimedMetadata_PASSTHROUGH = TimedMetadata' "PASSTHROUGH"

{-# COMPLETE
  TimedMetadata_NONE,
  TimedMetadata_PASSTHROUGH,
  TimedMetadata'
  #-}
