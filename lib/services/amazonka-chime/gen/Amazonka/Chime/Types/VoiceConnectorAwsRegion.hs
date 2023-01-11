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
-- Module      : Amazonka.Chime.Types.VoiceConnectorAwsRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VoiceConnectorAwsRegion
  ( VoiceConnectorAwsRegion
      ( ..,
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

pattern VoiceConnectorAwsRegion_Us_east_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_east_1 = VoiceConnectorAwsRegion' "us-east-1"

pattern VoiceConnectorAwsRegion_Us_west_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_west_2 = VoiceConnectorAwsRegion' "us-west-2"

{-# COMPLETE
  VoiceConnectorAwsRegion_Us_east_1,
  VoiceConnectorAwsRegion_Us_west_2,
  VoiceConnectorAwsRegion'
  #-}
