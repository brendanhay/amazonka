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
-- Module      : Amazonka.DirectoryService.Types.ClientAuthenticationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.ClientAuthenticationStatus
  ( ClientAuthenticationStatus
      ( ..,
        ClientAuthenticationStatus_Disabled,
        ClientAuthenticationStatus_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClientAuthenticationStatus = ClientAuthenticationStatus'
  { fromClientAuthenticationStatus ::
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

pattern ClientAuthenticationStatus_Disabled :: ClientAuthenticationStatus
pattern ClientAuthenticationStatus_Disabled = ClientAuthenticationStatus' "Disabled"

pattern ClientAuthenticationStatus_Enabled :: ClientAuthenticationStatus
pattern ClientAuthenticationStatus_Enabled = ClientAuthenticationStatus' "Enabled"

{-# COMPLETE
  ClientAuthenticationStatus_Disabled,
  ClientAuthenticationStatus_Enabled,
  ClientAuthenticationStatus'
  #-}
