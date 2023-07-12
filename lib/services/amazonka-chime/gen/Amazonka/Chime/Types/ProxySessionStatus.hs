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
-- Module      : Amazonka.Chime.Types.ProxySessionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ProxySessionStatus
  ( ProxySessionStatus
      ( ..,
        ProxySessionStatus_Closed,
        ProxySessionStatus_InProgress,
        ProxySessionStatus_Open
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProxySessionStatus = ProxySessionStatus'
  { fromProxySessionStatus ::
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

pattern ProxySessionStatus_Closed :: ProxySessionStatus
pattern ProxySessionStatus_Closed = ProxySessionStatus' "Closed"

pattern ProxySessionStatus_InProgress :: ProxySessionStatus
pattern ProxySessionStatus_InProgress = ProxySessionStatus' "InProgress"

pattern ProxySessionStatus_Open :: ProxySessionStatus
pattern ProxySessionStatus_Open = ProxySessionStatus' "Open"

{-# COMPLETE
  ProxySessionStatus_Closed,
  ProxySessionStatus_InProgress,
  ProxySessionStatus_Open,
  ProxySessionStatus'
  #-}
