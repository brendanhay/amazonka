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
-- Module      : Network.AWS.EC2.Types.VolumeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeType
  ( VolumeType
      ( ..,
        VolumeType_Gp2,
        VolumeType_Gp3,
        VolumeType_Io1,
        VolumeType_Io2,
        VolumeType_Sc1,
        VolumeType_St1,
        VolumeType_Standard
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VolumeType = VolumeType'
  { fromVolumeType ::
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

pattern VolumeType_Gp2 :: VolumeType
pattern VolumeType_Gp2 = VolumeType' "gp2"

pattern VolumeType_Gp3 :: VolumeType
pattern VolumeType_Gp3 = VolumeType' "gp3"

pattern VolumeType_Io1 :: VolumeType
pattern VolumeType_Io1 = VolumeType' "io1"

pattern VolumeType_Io2 :: VolumeType
pattern VolumeType_Io2 = VolumeType' "io2"

pattern VolumeType_Sc1 :: VolumeType
pattern VolumeType_Sc1 = VolumeType' "sc1"

pattern VolumeType_St1 :: VolumeType
pattern VolumeType_St1 = VolumeType' "st1"

pattern VolumeType_Standard :: VolumeType
pattern VolumeType_Standard = VolumeType' "standard"

{-# COMPLETE
  VolumeType_Gp2,
  VolumeType_Gp3,
  VolumeType_Io1,
  VolumeType_Io2,
  VolumeType_Sc1,
  VolumeType_St1,
  VolumeType_Standard,
  VolumeType'
  #-}
