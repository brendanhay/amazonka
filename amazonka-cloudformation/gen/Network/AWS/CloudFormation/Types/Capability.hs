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
-- Module      : Network.AWS.CloudFormation.Types.Capability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Capability
  ( Capability
      ( ..,
        Capability_CAPABILITY_AUTO_EXPAND,
        Capability_CAPABILITY_IAM,
        Capability_CAPABILITY_NAMED_IAM
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Capability = Capability'
  { fromCapability ::
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
