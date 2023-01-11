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
-- Module      : Amazonka.EC2.Types.ImageAttributeName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImageAttributeName
  ( ImageAttributeName
      ( ..,
        ImageAttributeName_BlockDeviceMapping,
        ImageAttributeName_BootMode,
        ImageAttributeName_Description,
        ImageAttributeName_ImdsSupport,
        ImageAttributeName_Kernel,
        ImageAttributeName_LastLaunchedTime,
        ImageAttributeName_LaunchPermission,
        ImageAttributeName_ProductCodes,
        ImageAttributeName_Ramdisk,
        ImageAttributeName_SriovNetSupport,
        ImageAttributeName_TpmSupport,
        ImageAttributeName_UefiData
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ImageAttributeName = ImageAttributeName'
  { fromImageAttributeName ::
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

pattern ImageAttributeName_BlockDeviceMapping :: ImageAttributeName
pattern ImageAttributeName_BlockDeviceMapping = ImageAttributeName' "blockDeviceMapping"

pattern ImageAttributeName_BootMode :: ImageAttributeName
pattern ImageAttributeName_BootMode = ImageAttributeName' "bootMode"

pattern ImageAttributeName_Description :: ImageAttributeName
pattern ImageAttributeName_Description = ImageAttributeName' "description"

pattern ImageAttributeName_ImdsSupport :: ImageAttributeName
pattern ImageAttributeName_ImdsSupport = ImageAttributeName' "imdsSupport"

pattern ImageAttributeName_Kernel :: ImageAttributeName
pattern ImageAttributeName_Kernel = ImageAttributeName' "kernel"

pattern ImageAttributeName_LastLaunchedTime :: ImageAttributeName
pattern ImageAttributeName_LastLaunchedTime = ImageAttributeName' "lastLaunchedTime"

pattern ImageAttributeName_LaunchPermission :: ImageAttributeName
pattern ImageAttributeName_LaunchPermission = ImageAttributeName' "launchPermission"

pattern ImageAttributeName_ProductCodes :: ImageAttributeName
pattern ImageAttributeName_ProductCodes = ImageAttributeName' "productCodes"

pattern ImageAttributeName_Ramdisk :: ImageAttributeName
pattern ImageAttributeName_Ramdisk = ImageAttributeName' "ramdisk"

pattern ImageAttributeName_SriovNetSupport :: ImageAttributeName
pattern ImageAttributeName_SriovNetSupport = ImageAttributeName' "sriovNetSupport"

pattern ImageAttributeName_TpmSupport :: ImageAttributeName
pattern ImageAttributeName_TpmSupport = ImageAttributeName' "tpmSupport"

pattern ImageAttributeName_UefiData :: ImageAttributeName
pattern ImageAttributeName_UefiData = ImageAttributeName' "uefiData"

{-# COMPLETE
  ImageAttributeName_BlockDeviceMapping,
  ImageAttributeName_BootMode,
  ImageAttributeName_Description,
  ImageAttributeName_ImdsSupport,
  ImageAttributeName_Kernel,
  ImageAttributeName_LastLaunchedTime,
  ImageAttributeName_LaunchPermission,
  ImageAttributeName_ProductCodes,
  ImageAttributeName_Ramdisk,
  ImageAttributeName_SriovNetSupport,
  ImageAttributeName_TpmSupport,
  ImageAttributeName_UefiData,
  ImageAttributeName'
  #-}
