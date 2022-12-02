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
-- Module      : Amazonka.Chime.Types.TranscribeMedicalRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.TranscribeMedicalRegion
  ( TranscribeMedicalRegion
      ( ..,
        TranscribeMedicalRegion_Ap_southeast_2,
        TranscribeMedicalRegion_Auto,
        TranscribeMedicalRegion_Ca_central_1,
        TranscribeMedicalRegion_Eu_west_1,
        TranscribeMedicalRegion_Us_east_1,
        TranscribeMedicalRegion_Us_east_2,
        TranscribeMedicalRegion_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribeMedicalRegion = TranscribeMedicalRegion'
  { fromTranscribeMedicalRegion ::
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

pattern TranscribeMedicalRegion_Ap_southeast_2 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Ap_southeast_2 = TranscribeMedicalRegion' "ap-southeast-2"

pattern TranscribeMedicalRegion_Auto :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Auto = TranscribeMedicalRegion' "auto"

pattern TranscribeMedicalRegion_Ca_central_1 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Ca_central_1 = TranscribeMedicalRegion' "ca-central-1"

pattern TranscribeMedicalRegion_Eu_west_1 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Eu_west_1 = TranscribeMedicalRegion' "eu-west-1"

pattern TranscribeMedicalRegion_Us_east_1 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Us_east_1 = TranscribeMedicalRegion' "us-east-1"

pattern TranscribeMedicalRegion_Us_east_2 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Us_east_2 = TranscribeMedicalRegion' "us-east-2"

pattern TranscribeMedicalRegion_Us_west_2 :: TranscribeMedicalRegion
pattern TranscribeMedicalRegion_Us_west_2 = TranscribeMedicalRegion' "us-west-2"

{-# COMPLETE
  TranscribeMedicalRegion_Ap_southeast_2,
  TranscribeMedicalRegion_Auto,
  TranscribeMedicalRegion_Ca_central_1,
  TranscribeMedicalRegion_Eu_west_1,
  TranscribeMedicalRegion_Us_east_1,
  TranscribeMedicalRegion_Us_east_2,
  TranscribeMedicalRegion_Us_west_2,
  TranscribeMedicalRegion'
  #-}
