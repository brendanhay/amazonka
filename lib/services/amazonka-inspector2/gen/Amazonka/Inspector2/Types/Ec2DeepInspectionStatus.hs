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
-- Module      : Amazonka.Inspector2.Types.Ec2DeepInspectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2DeepInspectionStatus
  ( Ec2DeepInspectionStatus
      ( ..,
        Ec2DeepInspectionStatus_ACTIVATED,
        Ec2DeepInspectionStatus_DEACTIVATED,
        Ec2DeepInspectionStatus_FAILED,
        Ec2DeepInspectionStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Ec2DeepInspectionStatus = Ec2DeepInspectionStatus'
  { fromEc2DeepInspectionStatus ::
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

pattern Ec2DeepInspectionStatus_ACTIVATED :: Ec2DeepInspectionStatus
pattern Ec2DeepInspectionStatus_ACTIVATED = Ec2DeepInspectionStatus' "ACTIVATED"

pattern Ec2DeepInspectionStatus_DEACTIVATED :: Ec2DeepInspectionStatus
pattern Ec2DeepInspectionStatus_DEACTIVATED = Ec2DeepInspectionStatus' "DEACTIVATED"

pattern Ec2DeepInspectionStatus_FAILED :: Ec2DeepInspectionStatus
pattern Ec2DeepInspectionStatus_FAILED = Ec2DeepInspectionStatus' "FAILED"

pattern Ec2DeepInspectionStatus_PENDING :: Ec2DeepInspectionStatus
pattern Ec2DeepInspectionStatus_PENDING = Ec2DeepInspectionStatus' "PENDING"

{-# COMPLETE
  Ec2DeepInspectionStatus_ACTIVATED,
  Ec2DeepInspectionStatus_DEACTIVATED,
  Ec2DeepInspectionStatus_FAILED,
  Ec2DeepInspectionStatus_PENDING,
  Ec2DeepInspectionStatus'
  #-}
