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
-- Module      : Amazonka.FSx.Types.WindowsDeploymentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.WindowsDeploymentType
  ( WindowsDeploymentType
      ( ..,
        WindowsDeploymentType_MULTI_AZ_1,
        WindowsDeploymentType_SINGLE_AZ_1,
        WindowsDeploymentType_SINGLE_AZ_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WindowsDeploymentType = WindowsDeploymentType'
  { fromWindowsDeploymentType ::
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

pattern WindowsDeploymentType_MULTI_AZ_1 :: WindowsDeploymentType
pattern WindowsDeploymentType_MULTI_AZ_1 = WindowsDeploymentType' "MULTI_AZ_1"

pattern WindowsDeploymentType_SINGLE_AZ_1 :: WindowsDeploymentType
pattern WindowsDeploymentType_SINGLE_AZ_1 = WindowsDeploymentType' "SINGLE_AZ_1"

pattern WindowsDeploymentType_SINGLE_AZ_2 :: WindowsDeploymentType
pattern WindowsDeploymentType_SINGLE_AZ_2 = WindowsDeploymentType' "SINGLE_AZ_2"

{-# COMPLETE
  WindowsDeploymentType_MULTI_AZ_1,
  WindowsDeploymentType_SINGLE_AZ_1,
  WindowsDeploymentType_SINGLE_AZ_2,
  WindowsDeploymentType'
  #-}
