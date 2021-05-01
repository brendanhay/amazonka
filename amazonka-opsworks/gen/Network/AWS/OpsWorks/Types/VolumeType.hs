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
-- Module      : Network.AWS.OpsWorks.Types.VolumeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.VolumeType
  ( VolumeType
      ( ..,
        VolumeType_Gp2,
        VolumeType_Io1,
        VolumeType_Standard
      ),
  )
where

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

pattern VolumeType_Io1 :: VolumeType
pattern VolumeType_Io1 = VolumeType' "io1"

pattern VolumeType_Standard :: VolumeType
pattern VolumeType_Standard = VolumeType' "standard"

{-# COMPLETE
  VolumeType_Gp2,
  VolumeType_Io1,
  VolumeType_Standard,
  VolumeType'
  #-}
