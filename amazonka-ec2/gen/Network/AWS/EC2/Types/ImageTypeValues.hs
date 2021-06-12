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
-- Module      : Network.AWS.EC2.Types.ImageTypeValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageTypeValues
  ( ImageTypeValues
      ( ..,
        ImageTypeValues_Kernel,
        ImageTypeValues_Machine,
        ImageTypeValues_Ramdisk
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ImageTypeValues = ImageTypeValues'
  { fromImageTypeValues ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ImageTypeValues_Kernel :: ImageTypeValues
pattern ImageTypeValues_Kernel = ImageTypeValues' "kernel"

pattern ImageTypeValues_Machine :: ImageTypeValues
pattern ImageTypeValues_Machine = ImageTypeValues' "machine"

pattern ImageTypeValues_Ramdisk :: ImageTypeValues
pattern ImageTypeValues_Ramdisk = ImageTypeValues' "ramdisk"

{-# COMPLETE
  ImageTypeValues_Kernel,
  ImageTypeValues_Machine,
  ImageTypeValues_Ramdisk,
  ImageTypeValues'
  #-}
