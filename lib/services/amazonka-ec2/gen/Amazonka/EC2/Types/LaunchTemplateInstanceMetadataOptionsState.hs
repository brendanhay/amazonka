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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
  ( LaunchTemplateInstanceMetadataOptionsState
      ( ..,
        LaunchTemplateInstanceMetadataOptionsState_Applied,
        LaunchTemplateInstanceMetadataOptionsState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype LaunchTemplateInstanceMetadataOptionsState = LaunchTemplateInstanceMetadataOptionsState'
  { fromLaunchTemplateInstanceMetadataOptionsState ::
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

pattern LaunchTemplateInstanceMetadataOptionsState_Applied :: LaunchTemplateInstanceMetadataOptionsState
pattern LaunchTemplateInstanceMetadataOptionsState_Applied = LaunchTemplateInstanceMetadataOptionsState' "applied"

pattern LaunchTemplateInstanceMetadataOptionsState_Pending :: LaunchTemplateInstanceMetadataOptionsState
pattern LaunchTemplateInstanceMetadataOptionsState_Pending = LaunchTemplateInstanceMetadataOptionsState' "pending"

{-# COMPLETE
  LaunchTemplateInstanceMetadataOptionsState_Applied,
  LaunchTemplateInstanceMetadataOptionsState_Pending,
  LaunchTemplateInstanceMetadataOptionsState'
  #-}
