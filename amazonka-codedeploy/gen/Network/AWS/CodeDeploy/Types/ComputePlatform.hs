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
-- Module      : Network.AWS.CodeDeploy.Types.ComputePlatform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ComputePlatform
  ( ComputePlatform
      ( ..,
        ComputePlatform_ECS,
        ComputePlatform_Lambda,
        ComputePlatform_Server
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ComputePlatform = ComputePlatform'
  { fromComputePlatform ::
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

pattern ComputePlatform_ECS :: ComputePlatform
pattern ComputePlatform_ECS = ComputePlatform' "ECS"

pattern ComputePlatform_Lambda :: ComputePlatform
pattern ComputePlatform_Lambda = ComputePlatform' "Lambda"

pattern ComputePlatform_Server :: ComputePlatform
pattern ComputePlatform_Server = ComputePlatform' "Server"

{-# COMPLETE
  ComputePlatform_ECS,
  ComputePlatform_Lambda,
  ComputePlatform_Server,
  ComputePlatform'
  #-}
