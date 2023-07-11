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
-- Module      : Amazonka.EC2.Types.VolumeStatusInfoStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeStatusInfoStatus
  ( VolumeStatusInfoStatus
      ( ..,
        VolumeStatusInfoStatus_Impaired,
        VolumeStatusInfoStatus_Insufficient_data,
        VolumeStatusInfoStatus_Ok
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VolumeStatusInfoStatus = VolumeStatusInfoStatus'
  { fromVolumeStatusInfoStatus ::
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

pattern VolumeStatusInfoStatus_Impaired :: VolumeStatusInfoStatus
pattern VolumeStatusInfoStatus_Impaired = VolumeStatusInfoStatus' "impaired"

pattern VolumeStatusInfoStatus_Insufficient_data :: VolumeStatusInfoStatus
pattern VolumeStatusInfoStatus_Insufficient_data = VolumeStatusInfoStatus' "insufficient-data"

pattern VolumeStatusInfoStatus_Ok :: VolumeStatusInfoStatus
pattern VolumeStatusInfoStatus_Ok = VolumeStatusInfoStatus' "ok"

{-# COMPLETE
  VolumeStatusInfoStatus_Impaired,
  VolumeStatusInfoStatus_Insufficient_data,
  VolumeStatusInfoStatus_Ok,
  VolumeStatusInfoStatus'
  #-}
