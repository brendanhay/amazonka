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
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Capability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Capability
  ( Capability
      ( ..,
        Capability_CAPABILITY_AUTO_EXPAND,
        Capability_CAPABILITY_IAM,
        Capability_CAPABILITY_NAMED_IAM,
        Capability_CAPABILITY_RESOURCE_POLICY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Values that must be specified in order to deploy some applications.
newtype Capability = Capability'
  { fromCapability ::
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

pattern Capability_CAPABILITY_AUTO_EXPAND :: Capability
pattern Capability_CAPABILITY_AUTO_EXPAND = Capability' "CAPABILITY_AUTO_EXPAND"

pattern Capability_CAPABILITY_IAM :: Capability
pattern Capability_CAPABILITY_IAM = Capability' "CAPABILITY_IAM"

pattern Capability_CAPABILITY_NAMED_IAM :: Capability
pattern Capability_CAPABILITY_NAMED_IAM = Capability' "CAPABILITY_NAMED_IAM"

pattern Capability_CAPABILITY_RESOURCE_POLICY :: Capability
pattern Capability_CAPABILITY_RESOURCE_POLICY = Capability' "CAPABILITY_RESOURCE_POLICY"

{-# COMPLETE
  Capability_CAPABILITY_AUTO_EXPAND,
  Capability_CAPABILITY_IAM,
  Capability_CAPABILITY_NAMED_IAM,
  Capability_CAPABILITY_RESOURCE_POLICY,
  Capability'
  #-}
