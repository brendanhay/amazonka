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
-- Module      : Network.AWS.AppFlow.Types.PrivateConnectionProvisioningFailureCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.PrivateConnectionProvisioningFailureCause
  ( PrivateConnectionProvisioningFailureCause
      ( ..,
        PrivateConnectionProvisioningFailureCause_ACCESS_DENIED,
        PrivateConnectionProvisioningFailureCause_CONNECTOR_AUTHENTICATION,
        PrivateConnectionProvisioningFailureCause_CONNECTOR_SERVER,
        PrivateConnectionProvisioningFailureCause_INTERNAL_SERVER,
        PrivateConnectionProvisioningFailureCause_VALIDATION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PrivateConnectionProvisioningFailureCause = PrivateConnectionProvisioningFailureCause'
  { fromPrivateConnectionProvisioningFailureCause ::
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

pattern PrivateConnectionProvisioningFailureCause_ACCESS_DENIED :: PrivateConnectionProvisioningFailureCause
pattern PrivateConnectionProvisioningFailureCause_ACCESS_DENIED = PrivateConnectionProvisioningFailureCause' "ACCESS_DENIED"

pattern PrivateConnectionProvisioningFailureCause_CONNECTOR_AUTHENTICATION :: PrivateConnectionProvisioningFailureCause
pattern PrivateConnectionProvisioningFailureCause_CONNECTOR_AUTHENTICATION = PrivateConnectionProvisioningFailureCause' "CONNECTOR_AUTHENTICATION"

pattern PrivateConnectionProvisioningFailureCause_CONNECTOR_SERVER :: PrivateConnectionProvisioningFailureCause
pattern PrivateConnectionProvisioningFailureCause_CONNECTOR_SERVER = PrivateConnectionProvisioningFailureCause' "CONNECTOR_SERVER"

pattern PrivateConnectionProvisioningFailureCause_INTERNAL_SERVER :: PrivateConnectionProvisioningFailureCause
pattern PrivateConnectionProvisioningFailureCause_INTERNAL_SERVER = PrivateConnectionProvisioningFailureCause' "INTERNAL_SERVER"

pattern PrivateConnectionProvisioningFailureCause_VALIDATION :: PrivateConnectionProvisioningFailureCause
pattern PrivateConnectionProvisioningFailureCause_VALIDATION = PrivateConnectionProvisioningFailureCause' "VALIDATION"

{-# COMPLETE
  PrivateConnectionProvisioningFailureCause_ACCESS_DENIED,
  PrivateConnectionProvisioningFailureCause_CONNECTOR_AUTHENTICATION,
  PrivateConnectionProvisioningFailureCause_CONNECTOR_SERVER,
  PrivateConnectionProvisioningFailureCause_INTERNAL_SERVER,
  PrivateConnectionProvisioningFailureCause_VALIDATION,
  PrivateConnectionProvisioningFailureCause'
  #-}
