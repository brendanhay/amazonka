{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageAttributeName
  ( ImageAttributeName
      ( ImageAttributeName',
        ImageAttributeNameDescription,
        ImageAttributeNameKernel,
        ImageAttributeNameRamdisk,
        ImageAttributeNameLaunchPermission,
        ImageAttributeNameProductCodes,
        ImageAttributeNameBlockDeviceMapping,
        ImageAttributeNameSriovNetSupport,
        fromImageAttributeName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImageAttributeName = ImageAttributeName'
  { fromImageAttributeName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ImageAttributeNameDescription :: ImageAttributeName
pattern ImageAttributeNameDescription = ImageAttributeName' "description"

pattern ImageAttributeNameKernel :: ImageAttributeName
pattern ImageAttributeNameKernel = ImageAttributeName' "kernel"

pattern ImageAttributeNameRamdisk :: ImageAttributeName
pattern ImageAttributeNameRamdisk = ImageAttributeName' "ramdisk"

pattern ImageAttributeNameLaunchPermission :: ImageAttributeName
pattern ImageAttributeNameLaunchPermission = ImageAttributeName' "launchPermission"

pattern ImageAttributeNameProductCodes :: ImageAttributeName
pattern ImageAttributeNameProductCodes = ImageAttributeName' "productCodes"

pattern ImageAttributeNameBlockDeviceMapping :: ImageAttributeName
pattern ImageAttributeNameBlockDeviceMapping = ImageAttributeName' "blockDeviceMapping"

pattern ImageAttributeNameSriovNetSupport :: ImageAttributeName
pattern ImageAttributeNameSriovNetSupport = ImageAttributeName' "sriovNetSupport"

{-# COMPLETE
  ImageAttributeNameDescription,
  ImageAttributeNameKernel,
  ImageAttributeNameRamdisk,
  ImageAttributeNameLaunchPermission,
  ImageAttributeNameProductCodes,
  ImageAttributeNameBlockDeviceMapping,
  ImageAttributeNameSriovNetSupport,
  ImageAttributeName'
  #-}
