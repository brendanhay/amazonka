{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.DependentServiceName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.DependentServiceName
  ( DependentServiceName
      ( ..,
        DependentServiceName_AWSCONFIG,
        DependentServiceName_AWSSHIELD_ADVANCED,
        DependentServiceName_AWSVPC,
        DependentServiceName_AWSWAF
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DependentServiceName = DependentServiceName'
  { fromDependentServiceName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
