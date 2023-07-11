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
-- Module      : Amazonka.ImageBuilder.Types.ImageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageStatus
  ( ImageStatus
      ( ..,
        ImageStatus_AVAILABLE,
        ImageStatus_BUILDING,
        ImageStatus_CANCELLED,
        ImageStatus_CREATING,
        ImageStatus_DELETED,
        ImageStatus_DEPRECATED,
        ImageStatus_DISTRIBUTING,
        ImageStatus_FAILED,
        ImageStatus_INTEGRATING,
        ImageStatus_PENDING,
        ImageStatus_TESTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageStatus = ImageStatus'
  { fromImageStatus ::
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

pattern ImageStatus_AVAILABLE :: ImageStatus
pattern ImageStatus_AVAILABLE = ImageStatus' "AVAILABLE"

pattern ImageStatus_BUILDING :: ImageStatus
pattern ImageStatus_BUILDING = ImageStatus' "BUILDING"

pattern ImageStatus_CANCELLED :: ImageStatus
pattern ImageStatus_CANCELLED = ImageStatus' "CANCELLED"

pattern ImageStatus_CREATING :: ImageStatus
pattern ImageStatus_CREATING = ImageStatus' "CREATING"

pattern ImageStatus_DELETED :: ImageStatus
pattern ImageStatus_DELETED = ImageStatus' "DELETED"

pattern ImageStatus_DEPRECATED :: ImageStatus
pattern ImageStatus_DEPRECATED = ImageStatus' "DEPRECATED"

pattern ImageStatus_DISTRIBUTING :: ImageStatus
pattern ImageStatus_DISTRIBUTING = ImageStatus' "DISTRIBUTING"

pattern ImageStatus_FAILED :: ImageStatus
pattern ImageStatus_FAILED = ImageStatus' "FAILED"

pattern ImageStatus_INTEGRATING :: ImageStatus
pattern ImageStatus_INTEGRATING = ImageStatus' "INTEGRATING"

pattern ImageStatus_PENDING :: ImageStatus
pattern ImageStatus_PENDING = ImageStatus' "PENDING"

pattern ImageStatus_TESTING :: ImageStatus
pattern ImageStatus_TESTING = ImageStatus' "TESTING"

{-# COMPLETE
  ImageStatus_AVAILABLE,
  ImageStatus_BUILDING,
  ImageStatus_CANCELLED,
  ImageStatus_CREATING,
  ImageStatus_DELETED,
  ImageStatus_DEPRECATED,
  ImageStatus_DISTRIBUTING,
  ImageStatus_FAILED,
  ImageStatus_INTEGRATING,
  ImageStatus_PENDING,
  ImageStatus_TESTING,
  ImageStatus'
  #-}
