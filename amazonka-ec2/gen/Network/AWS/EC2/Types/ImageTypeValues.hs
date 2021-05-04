{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ImageTypeValues = ImageTypeValues'
  { fromImageTypeValues ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
