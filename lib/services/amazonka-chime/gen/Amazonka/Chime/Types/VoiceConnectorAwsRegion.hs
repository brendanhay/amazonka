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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype VoiceConnectorAwsRegion = VoiceConnectorAwsRegion'
  { fromVoiceConnectorAwsRegion ::
      Core.Text
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

pattern VoiceConnectorAwsRegion_Us_east_1 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_east_1 = VoiceConnectorAwsRegion' "us-east-1"

pattern VoiceConnectorAwsRegion_Us_west_2 :: VoiceConnectorAwsRegion
pattern VoiceConnectorAwsRegion_Us_west_2 = VoiceConnectorAwsRegion' "us-west-2"

{-# COMPLETE
  VoiceConnectorAwsRegion_Us_east_1,
  VoiceConnectorAwsRegion_Us_west_2,
  VoiceConnectorAwsRegion'
  #-}
