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
-- Module      : Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationType
  ( KafkaClusterClientAuthenticationType
      ( ..,
        KafkaClusterClientAuthenticationType_IAM,
        KafkaClusterClientAuthenticationType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KafkaClusterClientAuthenticationType = KafkaClusterClientAuthenticationType'
  { fromKafkaClusterClientAuthenticationType ::
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

pattern KafkaClusterClientAuthenticationType_IAM :: KafkaClusterClientAuthenticationType
pattern KafkaClusterClientAuthenticationType_IAM = KafkaClusterClientAuthenticationType' "IAM"

pattern KafkaClusterClientAuthenticationType_NONE :: KafkaClusterClientAuthenticationType
pattern KafkaClusterClientAuthenticationType_NONE = KafkaClusterClientAuthenticationType' "NONE"

{-# COMPLETE
  KafkaClusterClientAuthenticationType_IAM,
  KafkaClusterClientAuthenticationType_NONE,
  KafkaClusterClientAuthenticationType'
  #-}
