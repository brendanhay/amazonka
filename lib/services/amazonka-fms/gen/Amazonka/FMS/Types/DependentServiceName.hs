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
-- Module      : Amazonka.FMS.Types.DependentServiceName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DependentServiceName
  ( DependentServiceName
      ( ..,
        DependentServiceName_AWSCONFIG,
        DependentServiceName_AWSSHIELD_ADVANCED,
        DependentServiceName_AWSVPC,
        DependentServiceName_AWSWAF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DependentServiceName = DependentServiceName'
  { fromDependentServiceName ::
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

pattern DependentServiceName_AWSCONFIG :: DependentServiceName
pattern DependentServiceName_AWSCONFIG = DependentServiceName' "AWSCONFIG"

pattern DependentServiceName_AWSSHIELD_ADVANCED :: DependentServiceName
pattern DependentServiceName_AWSSHIELD_ADVANCED = DependentServiceName' "AWSSHIELD_ADVANCED"

pattern DependentServiceName_AWSVPC :: DependentServiceName
pattern DependentServiceName_AWSVPC = DependentServiceName' "AWSVPC"

pattern DependentServiceName_AWSWAF :: DependentServiceName
pattern DependentServiceName_AWSWAF = DependentServiceName' "AWSWAF"

{-# COMPLETE
  DependentServiceName_AWSCONFIG,
  DependentServiceName_AWSSHIELD_ADVANCED,
  DependentServiceName_AWSVPC,
  DependentServiceName_AWSWAF,
  DependentServiceName'
  #-}
