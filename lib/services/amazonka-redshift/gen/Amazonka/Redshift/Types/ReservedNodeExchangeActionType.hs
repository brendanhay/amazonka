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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype ReservedNodeExchangeActionType = ReservedNodeExchangeActionType'
  { fromReservedNodeExchangeActionType ::
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

pattern ReservedNodeExchangeActionType_Resize_cluster :: ReservedNodeExchangeActionType
pattern ReservedNodeExchangeActionType_Resize_cluster = ReservedNodeExchangeActionType' "resize-cluster"

pattern ReservedNodeExchangeActionType_Restore_cluster :: ReservedNodeExchangeActionType
pattern ReservedNodeExchangeActionType_Restore_cluster = ReservedNodeExchangeActionType' "restore-cluster"

{-# COMPLETE
  ReservedNodeExchangeActionType_Resize_cluster,
  ReservedNodeExchangeActionType_Restore_cluster,
  ReservedNodeExchangeActionType'
  #-}
