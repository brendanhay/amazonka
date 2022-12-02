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
-- Module      : Amazonka.ElastiCache.Types.PendingAutomaticFailoverStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.PendingAutomaticFailoverStatus
  ( PendingAutomaticFailoverStatus
      ( ..,
        PendingAutomaticFailoverStatus_Disabled,
        PendingAutomaticFailoverStatus_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus'
  { fromPendingAutomaticFailoverStatus ::
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

pattern PendingAutomaticFailoverStatus_Disabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Disabled = PendingAutomaticFailoverStatus' "disabled"

pattern PendingAutomaticFailoverStatus_Enabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Enabled = PendingAutomaticFailoverStatus' "enabled"

{-# COMPLETE
  PendingAutomaticFailoverStatus_Disabled,
  PendingAutomaticFailoverStatus_Enabled,
  PendingAutomaticFailoverStatus'
  #-}
