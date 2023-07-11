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
-- Module      : Amazonka.ChimeSdkMeetings.Types.TranscribeRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.TranscribeRegion
  ( TranscribeRegion
      ( ..,
        TranscribeRegion_Ap_northeast_1,
        TranscribeRegion_Ap_northeast_2,
        TranscribeRegion_Ap_southeast_2,
        TranscribeRegion_Auto,
        TranscribeRegion_Ca_central_1,
        TranscribeRegion_Eu_central_1,
        TranscribeRegion_Eu_west_1,
        TranscribeRegion_Eu_west_2,
        TranscribeRegion_Sa_east_1,
        TranscribeRegion_Us_east_1,
        TranscribeRegion_Us_east_2,
        TranscribeRegion_Us_gov_west_1,
        TranscribeRegion_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribeRegion = TranscribeRegion'
  { fromTranscribeRegion ::
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

pattern TranscribeRegion_Ap_northeast_1 :: TranscribeRegion
pattern TranscribeRegion_Ap_northeast_1 = TranscribeRegion' "ap-northeast-1"

pattern TranscribeRegion_Ap_northeast_2 :: TranscribeRegion
pattern TranscribeRegion_Ap_northeast_2 = TranscribeRegion' "ap-northeast-2"

pattern TranscribeRegion_Ap_southeast_2 :: TranscribeRegion
pattern TranscribeRegion_Ap_southeast_2 = TranscribeRegion' "ap-southeast-2"

pattern TranscribeRegion_Auto :: TranscribeRegion
pattern TranscribeRegion_Auto = TranscribeRegion' "auto"

pattern TranscribeRegion_Ca_central_1 :: TranscribeRegion
pattern TranscribeRegion_Ca_central_1 = TranscribeRegion' "ca-central-1"

pattern TranscribeRegion_Eu_central_1 :: TranscribeRegion
pattern TranscribeRegion_Eu_central_1 = TranscribeRegion' "eu-central-1"

pattern TranscribeRegion_Eu_west_1 :: TranscribeRegion
pattern TranscribeRegion_Eu_west_1 = TranscribeRegion' "eu-west-1"

pattern TranscribeRegion_Eu_west_2 :: TranscribeRegion
pattern TranscribeRegion_Eu_west_2 = TranscribeRegion' "eu-west-2"

pattern TranscribeRegion_Sa_east_1 :: TranscribeRegion
pattern TranscribeRegion_Sa_east_1 = TranscribeRegion' "sa-east-1"

pattern TranscribeRegion_Us_east_1 :: TranscribeRegion
pattern TranscribeRegion_Us_east_1 = TranscribeRegion' "us-east-1"

pattern TranscribeRegion_Us_east_2 :: TranscribeRegion
pattern TranscribeRegion_Us_east_2 = TranscribeRegion' "us-east-2"

pattern TranscribeRegion_Us_gov_west_1 :: TranscribeRegion
pattern TranscribeRegion_Us_gov_west_1 = TranscribeRegion' "us-gov-west-1"

pattern TranscribeRegion_Us_west_2 :: TranscribeRegion
pattern TranscribeRegion_Us_west_2 = TranscribeRegion' "us-west-2"

{-# COMPLETE
  TranscribeRegion_Ap_northeast_1,
  TranscribeRegion_Ap_northeast_2,
  TranscribeRegion_Ap_southeast_2,
  TranscribeRegion_Auto,
  TranscribeRegion_Ca_central_1,
  TranscribeRegion_Eu_central_1,
  TranscribeRegion_Eu_west_1,
  TranscribeRegion_Eu_west_2,
  TranscribeRegion_Sa_east_1,
  TranscribeRegion_Us_east_1,
  TranscribeRegion_Us_east_2,
  TranscribeRegion_Us_gov_west_1,
  TranscribeRegion_Us_west_2,
  TranscribeRegion'
  #-}
