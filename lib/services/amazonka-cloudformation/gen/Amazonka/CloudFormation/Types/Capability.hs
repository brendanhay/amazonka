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
-- Module      : Amazonka.CloudFormation.Types.Capability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.Capability
  ( Capability
      ( ..,
        Capability_CAPABILITY_AUTO_EXPAND,
        Capability_CAPABILITY_IAM,
        Capability_CAPABILITY_NAMED_IAM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Capability = Capability'
  { fromCapability ::
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

pattern Capability_CAPABILITY_AUTO_EXPAND :: Capability
pattern Capability_CAPABILITY_AUTO_EXPAND = Capability' "CAPABILITY_AUTO_EXPAND"

pattern Capability_CAPABILITY_IAM :: Capability
pattern Capability_CAPABILITY_IAM = Capability' "CAPABILITY_IAM"

pattern Capability_CAPABILITY_NAMED_IAM :: Capability
pattern Capability_CAPABILITY_NAMED_IAM = Capability' "CAPABILITY_NAMED_IAM"

{-# COMPLETE
  Capability_CAPABILITY_AUTO_EXPAND,
  Capability_CAPABILITY_IAM,
  Capability_CAPABILITY_NAMED_IAM,
  Capability'
  #-}
