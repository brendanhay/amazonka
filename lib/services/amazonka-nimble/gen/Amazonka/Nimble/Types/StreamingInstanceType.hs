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
-- Module      : Amazonka.Nimble.Types.StreamingInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingInstanceType
  ( StreamingInstanceType
      ( ..,
        StreamingInstanceType_G4dn_12xlarge,
        StreamingInstanceType_G4dn_16xlarge,
        StreamingInstanceType_G4dn_2xlarge,
        StreamingInstanceType_G4dn_4xlarge,
        StreamingInstanceType_G4dn_8xlarge,
        StreamingInstanceType_G4dn_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StreamingInstanceType = StreamingInstanceType'
  { fromStreamingInstanceType ::
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

pattern StreamingInstanceType_G4dn_12xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_12xlarge = StreamingInstanceType' "g4dn.12xlarge"

pattern StreamingInstanceType_G4dn_16xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_16xlarge = StreamingInstanceType' "g4dn.16xlarge"

pattern StreamingInstanceType_G4dn_2xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_2xlarge = StreamingInstanceType' "g4dn.2xlarge"

pattern StreamingInstanceType_G4dn_4xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_4xlarge = StreamingInstanceType' "g4dn.4xlarge"

pattern StreamingInstanceType_G4dn_8xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_8xlarge = StreamingInstanceType' "g4dn.8xlarge"

pattern StreamingInstanceType_G4dn_xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G4dn_xlarge = StreamingInstanceType' "g4dn.xlarge"

{-# COMPLETE
  StreamingInstanceType_G4dn_12xlarge,
  StreamingInstanceType_G4dn_16xlarge,
  StreamingInstanceType_G4dn_2xlarge,
  StreamingInstanceType_G4dn_4xlarge,
  StreamingInstanceType_G4dn_8xlarge,
  StreamingInstanceType_G4dn_xlarge,
  StreamingInstanceType'
  #-}
