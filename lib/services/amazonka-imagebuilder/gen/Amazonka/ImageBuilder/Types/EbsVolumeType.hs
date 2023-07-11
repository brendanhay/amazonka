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
-- Module      : Amazonka.ImageBuilder.Types.EbsVolumeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.EbsVolumeType
  ( EbsVolumeType
      ( ..,
        EbsVolumeType_Gp2,
        EbsVolumeType_Gp3,
        EbsVolumeType_Io1,
        EbsVolumeType_Io2,
        EbsVolumeType_Sc1,
        EbsVolumeType_St1,
        EbsVolumeType_Standard
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EbsVolumeType = EbsVolumeType'
  { fromEbsVolumeType ::
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

pattern EbsVolumeType_Gp2 :: EbsVolumeType
pattern EbsVolumeType_Gp2 = EbsVolumeType' "gp2"

pattern EbsVolumeType_Gp3 :: EbsVolumeType
pattern EbsVolumeType_Gp3 = EbsVolumeType' "gp3"

pattern EbsVolumeType_Io1 :: EbsVolumeType
pattern EbsVolumeType_Io1 = EbsVolumeType' "io1"

pattern EbsVolumeType_Io2 :: EbsVolumeType
pattern EbsVolumeType_Io2 = EbsVolumeType' "io2"

pattern EbsVolumeType_Sc1 :: EbsVolumeType
pattern EbsVolumeType_Sc1 = EbsVolumeType' "sc1"

pattern EbsVolumeType_St1 :: EbsVolumeType
pattern EbsVolumeType_St1 = EbsVolumeType' "st1"

pattern EbsVolumeType_Standard :: EbsVolumeType
pattern EbsVolumeType_Standard = EbsVolumeType' "standard"

{-# COMPLETE
  EbsVolumeType_Gp2,
  EbsVolumeType_Gp3,
  EbsVolumeType_Io1,
  EbsVolumeType_Io2,
  EbsVolumeType_Sc1,
  EbsVolumeType_St1,
  EbsVolumeType_Standard,
  EbsVolumeType'
  #-}
