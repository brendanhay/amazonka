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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceConnectorAwsRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceConnectorAwsRegion
  ( VoiceConnectorAwsRegion
      ( ..,
        VoiceConnectorAwsRegion_Ap_northeast_1,
        VoiceConnectorAwsRegion_Ap_northeast_2,
        VoiceConnectorAwsRegion_Ap_southeast_1,
        VoiceConnectorAwsRegion_Ap_southeast_2,
        VoiceConnectorAwsRegion_Ca_central_1,
        VoiceConnectorAwsRegion_Eu_central_1,
        VoiceConnectorAwsRegion_Eu_west_1,
        VoiceConnectorAwsRegion_Eu_west_2,
        VoiceConnectorAwsRegion_Us_east_1,
        VoiceConnectorAwsRegion_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VoiceConnectorAwsRegion = VoiceConnectorAwsRegion'
  { fromVoiceConnectorAwsRegion ::
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

pattern VoiceConnectorAwsRegion_Ap_northeast_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Ap_northeast_1 = VoiceConnectorAwsRegion' "ap-northeast-1"

pattern VoiceConnectorAwsRegion_Ap_northeast_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Ap_northeast_2 = VoiceConnectorAwsRegion' "ap-northeast-2"

pattern VoiceConnectorAwsRegion_Ap_southeast_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Ap_southeast_1 = VoiceConnectorAwsRegion' "ap-southeast-1"

pattern VoiceConnectorAwsRegion_Ap_southeast_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Ap_southeast_2 = VoiceConnectorAwsRegion' "ap-southeast-2"

pattern VoiceConnectorAwsRegion_Ca_central_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Ca_central_1 = VoiceConnectorAwsRegion' "ca-central-1"

pattern VoiceConnectorAwsRegion_Eu_central_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Eu_central_1 = VoiceConnectorAwsRegion' "eu-central-1"

pattern VoiceConnectorAwsRegion_Eu_west_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Eu_west_1 = VoiceConnectorAwsRegion' "eu-west-1"

pattern VoiceConnectorAwsRegion_Eu_west_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Eu_west_2 = VoiceConnectorAwsRegion' "eu-west-2"

pattern VoiceConnectorAwsRegion_Us_east_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_east_1 = VoiceConnectorAwsRegion' "us-east-1"

pattern VoiceConnectorAwsRegion_Us_west_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_west_2 = VoiceConnectorAwsRegion' "us-west-2"

{-# COMPLETE
  VoiceConnectorAwsRegion_Ap_northeast_1,
  VoiceConnectorAwsRegion_Ap_northeast_2,
  VoiceConnectorAwsRegion_Ap_southeast_1,
  VoiceConnectorAwsRegion_Ap_southeast_2,
  VoiceConnectorAwsRegion_Ca_central_1,
  VoiceConnectorAwsRegion_Eu_central_1,
  VoiceConnectorAwsRegion_Eu_west_1,
  VoiceConnectorAwsRegion_Eu_west_2,
  VoiceConnectorAwsRegion_Us_east_1,
  VoiceConnectorAwsRegion_Us_west_2,
  VoiceConnectorAwsRegion'
  #-}
