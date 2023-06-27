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
-- Module      : Amazonka.CleanRooms.Types.CollaborationQueryLogStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.CollaborationQueryLogStatus
  ( CollaborationQueryLogStatus
      ( ..,
        CollaborationQueryLogStatus_DISABLED,
        CollaborationQueryLogStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CollaborationQueryLogStatus = CollaborationQueryLogStatus'
  { fromCollaborationQueryLogStatus ::
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

pattern CollaborationQueryLogStatus_DISABLED :: CollaborationQueryLogStatus
pattern CollaborationQueryLogStatus_DISABLED = CollaborationQueryLogStatus' "DISABLED"

pattern CollaborationQueryLogStatus_ENABLED :: CollaborationQueryLogStatus
pattern CollaborationQueryLogStatus_ENABLED = CollaborationQueryLogStatus' "ENABLED"

{-# COMPLETE
  CollaborationQueryLogStatus_DISABLED,
  CollaborationQueryLogStatus_ENABLED,
  CollaborationQueryLogStatus'
  #-}
