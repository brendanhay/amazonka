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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHttpTokensState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHttpTokensState
  ( LaunchTemplateHttpTokensState
      ( ..,
        LaunchTemplateHttpTokensState_Optional,
        LaunchTemplateHttpTokensState_Required
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype LaunchTemplateHttpTokensState = LaunchTemplateHttpTokensState'
  { fromLaunchTemplateHttpTokensState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern LaunchTemplateHttpTokensState_Optional :: LaunchTemplateHttpTokensState
pattern LaunchTemplateHttpTokensState_Optional = LaunchTemplateHttpTokensState' "optional"

pattern LaunchTemplateHttpTokensState_Required :: LaunchTemplateHttpTokensState
pattern LaunchTemplateHttpTokensState_Required = LaunchTemplateHttpTokensState' "required"

{-# COMPLETE
  LaunchTemplateHttpTokensState_Optional,
  LaunchTemplateHttpTokensState_Required,
  LaunchTemplateHttpTokensState'
  #-}
