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
-- Module      : Amazonka.Redshift.Types.ReservedNodeExchangeActionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ReservedNodeExchangeActionType
  ( ReservedNodeExchangeActionType
      ( ..,
        ReservedNodeExchangeActionType_Resize_cluster,
        ReservedNodeExchangeActionType_Restore_cluster
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype ReservedNodeExchangeActionType = ReservedNodeExchangeActionType'
  { fromReservedNodeExchangeActionType ::
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

pattern ReservedNodeExchangeActionType_Resize_cluster :: ReservedNodeExchangeActionType
pattern ReservedNodeExchangeActionType_Resize_cluster = ReservedNodeExchangeActionType' "resize-cluster"

pattern ReservedNodeExchangeActionType_Restore_cluster :: ReservedNodeExchangeActionType
pattern ReservedNodeExchangeActionType_Restore_cluster = ReservedNodeExchangeActionType' "restore-cluster"

{-# COMPLETE
  ReservedNodeExchangeActionType_Resize_cluster,
  ReservedNodeExchangeActionType_Restore_cluster,
  ReservedNodeExchangeActionType'
  #-}
