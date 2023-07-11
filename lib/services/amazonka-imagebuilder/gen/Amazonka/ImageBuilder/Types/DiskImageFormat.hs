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
-- Module      : Amazonka.ImageBuilder.Types.DiskImageFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.DiskImageFormat
  ( DiskImageFormat
      ( ..,
        DiskImageFormat_RAW,
        DiskImageFormat_VHD,
        DiskImageFormat_VMDK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DiskImageFormat = DiskImageFormat'
  { fromDiskImageFormat ::
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

pattern DiskImageFormat_RAW :: DiskImageFormat
pattern DiskImageFormat_RAW = DiskImageFormat' "RAW"

pattern DiskImageFormat_VHD :: DiskImageFormat
pattern DiskImageFormat_VHD = DiskImageFormat' "VHD"

pattern DiskImageFormat_VMDK :: DiskImageFormat
pattern DiskImageFormat_VMDK = DiskImageFormat' "VMDK"

{-# COMPLETE
  DiskImageFormat_RAW,
  DiskImageFormat_VHD,
  DiskImageFormat_VMDK,
  DiskImageFormat'
  #-}
