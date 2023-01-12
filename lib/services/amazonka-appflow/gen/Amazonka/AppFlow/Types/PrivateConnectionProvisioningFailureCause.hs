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
-- Module      : Amazonka.AppFlow.Types.PrivateConnectionProvisioningFailureCause
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrivateConnectionProvisioningFailureCause
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrivateConnectionProvisioningFailureCause = PrivateConnectionProvisioningFailureCause'
  { fromPrivateConnectionProvisioningFailureCause ::
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
