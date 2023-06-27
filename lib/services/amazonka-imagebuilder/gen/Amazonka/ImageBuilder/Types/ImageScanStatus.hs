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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanStatus
  ( ImageScanStatus
      ( ..,
        ImageScanStatus_ABANDONED,
        ImageScanStatus_COLLECTING,
        ImageScanStatus_COMPLETED,
        ImageScanStatus_FAILED,
        ImageScanStatus_PENDING,
        ImageScanStatus_SCANNING,
        ImageScanStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageScanStatus = ImageScanStatus'
  { fromImageScanStatus ::
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

pattern ImageScanStatus_ABANDONED :: ImageScanStatus
pattern ImageScanStatus_ABANDONED = ImageScanStatus' "ABANDONED"

pattern ImageScanStatus_COLLECTING :: ImageScanStatus
pattern ImageScanStatus_COLLECTING = ImageScanStatus' "COLLECTING"

pattern ImageScanStatus_COMPLETED :: ImageScanStatus
pattern ImageScanStatus_COMPLETED = ImageScanStatus' "COMPLETED"

pattern ImageScanStatus_FAILED :: ImageScanStatus
pattern ImageScanStatus_FAILED = ImageScanStatus' "FAILED"

pattern ImageScanStatus_PENDING :: ImageScanStatus
pattern ImageScanStatus_PENDING = ImageScanStatus' "PENDING"

pattern ImageScanStatus_SCANNING :: ImageScanStatus
pattern ImageScanStatus_SCANNING = ImageScanStatus' "SCANNING"

pattern ImageScanStatus_TIMED_OUT :: ImageScanStatus
pattern ImageScanStatus_TIMED_OUT = ImageScanStatus' "TIMED_OUT"

{-# COMPLETE
  ImageScanStatus_ABANDONED,
  ImageScanStatus_COLLECTING,
  ImageScanStatus_COMPLETED,
  ImageScanStatus_FAILED,
  ImageScanStatus_PENDING,
  ImageScanStatus_SCANNING,
  ImageScanStatus_TIMED_OUT,
  ImageScanStatus'
  #-}
