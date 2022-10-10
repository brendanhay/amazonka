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
-- Module      : Amazonka.DrS.Types.RecoverySnapshotsOrder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoverySnapshotsOrder
  ( RecoverySnapshotsOrder
      ( ..,
        RecoverySnapshotsOrder_ASC,
        RecoverySnapshotsOrder_DESC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RecoverySnapshotsOrder = RecoverySnapshotsOrder'
  { fromRecoverySnapshotsOrder ::
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

pattern RecoverySnapshotsOrder_ASC :: RecoverySnapshotsOrder
pattern RecoverySnapshotsOrder_ASC = RecoverySnapshotsOrder' "ASC"

pattern RecoverySnapshotsOrder_DESC :: RecoverySnapshotsOrder
pattern RecoverySnapshotsOrder_DESC = RecoverySnapshotsOrder' "DESC"

{-# COMPLETE
  RecoverySnapshotsOrder_ASC,
  RecoverySnapshotsOrder_DESC,
  RecoverySnapshotsOrder'
  #-}
