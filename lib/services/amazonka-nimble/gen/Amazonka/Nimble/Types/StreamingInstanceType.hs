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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingInstanceType
  ( StreamingInstanceType
      ( ..,
        StreamingInstanceType_G3_4xlarge,
        StreamingInstanceType_G3s_xlarge,
        StreamingInstanceType_G4dn_12xlarge,
        StreamingInstanceType_G4dn_16xlarge,
        StreamingInstanceType_G4dn_2xlarge,
        StreamingInstanceType_G4dn_4xlarge,
        StreamingInstanceType_G4dn_8xlarge,
        StreamingInstanceType_G4dn_xlarge,
        StreamingInstanceType_G5_16xlarge,
        StreamingInstanceType_G5_2xlarge,
        StreamingInstanceType_G5_4xlarge,
        StreamingInstanceType_G5_8xlarge,
        StreamingInstanceType_G5_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamingInstanceType = StreamingInstanceType'
  { fromStreamingInstanceType ::
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

pattern StreamingInstanceType_G3_4xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G3_4xlarge = StreamingInstanceType' "g3.4xlarge"

pattern StreamingInstanceType_G3s_xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G3s_xlarge = StreamingInstanceType' "g3s.xlarge"

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

pattern StreamingInstanceType_G5_16xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G5_16xlarge = StreamingInstanceType' "g5.16xlarge"

pattern StreamingInstanceType_G5_2xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G5_2xlarge = StreamingInstanceType' "g5.2xlarge"

pattern StreamingInstanceType_G5_4xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G5_4xlarge = StreamingInstanceType' "g5.4xlarge"

pattern StreamingInstanceType_G5_8xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G5_8xlarge = StreamingInstanceType' "g5.8xlarge"

pattern StreamingInstanceType_G5_xlarge :: StreamingInstanceType
pattern StreamingInstanceType_G5_xlarge = StreamingInstanceType' "g5.xlarge"

{-# COMPLETE
  StreamingInstanceType_G3_4xlarge,
  StreamingInstanceType_G3s_xlarge,
  StreamingInstanceType_G4dn_12xlarge,
  StreamingInstanceType_G4dn_16xlarge,
  StreamingInstanceType_G4dn_2xlarge,
  StreamingInstanceType_G4dn_4xlarge,
  StreamingInstanceType_G4dn_8xlarge,
  StreamingInstanceType_G4dn_xlarge,
  StreamingInstanceType_G5_16xlarge,
  StreamingInstanceType_G5_2xlarge,
  StreamingInstanceType_G5_4xlarge,
  StreamingInstanceType_G5_8xlarge,
  StreamingInstanceType_G5_xlarge,
  StreamingInstanceType'
  #-}
