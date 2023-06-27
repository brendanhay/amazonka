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
-- Module      : Amazonka.EC2.Types.Ec2InstanceConnectEndpointState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ec2InstanceConnectEndpointState
  ( Ec2InstanceConnectEndpointState
      ( ..,
        Ec2InstanceConnectEndpointState_Create_complete,
        Ec2InstanceConnectEndpointState_Create_failed,
        Ec2InstanceConnectEndpointState_Create_in_progress,
        Ec2InstanceConnectEndpointState_Delete_complete,
        Ec2InstanceConnectEndpointState_Delete_failed,
        Ec2InstanceConnectEndpointState_Delete_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype Ec2InstanceConnectEndpointState = Ec2InstanceConnectEndpointState'
  { fromEc2InstanceConnectEndpointState ::
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

pattern Ec2InstanceConnectEndpointState_Create_complete :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Create_complete = Ec2InstanceConnectEndpointState' "create-complete"

pattern Ec2InstanceConnectEndpointState_Create_failed :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Create_failed = Ec2InstanceConnectEndpointState' "create-failed"

pattern Ec2InstanceConnectEndpointState_Create_in_progress :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Create_in_progress = Ec2InstanceConnectEndpointState' "create-in-progress"

pattern Ec2InstanceConnectEndpointState_Delete_complete :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Delete_complete = Ec2InstanceConnectEndpointState' "delete-complete"

pattern Ec2InstanceConnectEndpointState_Delete_failed :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Delete_failed = Ec2InstanceConnectEndpointState' "delete-failed"

pattern Ec2InstanceConnectEndpointState_Delete_in_progress :: Ec2InstanceConnectEndpointState
pattern Ec2InstanceConnectEndpointState_Delete_in_progress = Ec2InstanceConnectEndpointState' "delete-in-progress"

{-# COMPLETE
  Ec2InstanceConnectEndpointState_Create_complete,
  Ec2InstanceConnectEndpointState_Create_failed,
  Ec2InstanceConnectEndpointState_Create_in_progress,
  Ec2InstanceConnectEndpointState_Delete_complete,
  Ec2InstanceConnectEndpointState_Delete_failed,
  Ec2InstanceConnectEndpointState_Delete_in_progress,
  Ec2InstanceConnectEndpointState'
  #-}
