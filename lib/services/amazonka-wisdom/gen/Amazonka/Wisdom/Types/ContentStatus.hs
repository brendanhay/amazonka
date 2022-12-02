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
-- Module      : Amazonka.Wisdom.Types.ContentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.ContentStatus
  ( ContentStatus
      ( ..,
        ContentStatus_ACTIVE,
        ContentStatus_CREATE_FAILED,
        ContentStatus_CREATE_IN_PROGRESS,
        ContentStatus_DELETED,
        ContentStatus_DELETE_FAILED,
        ContentStatus_DELETE_IN_PROGRESS,
        ContentStatus_UPDATE_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentStatus = ContentStatus'
  { fromContentStatus ::
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

pattern ContentStatus_ACTIVE :: ContentStatus
pattern ContentStatus_ACTIVE = ContentStatus' "ACTIVE"

pattern ContentStatus_CREATE_FAILED :: ContentStatus
pattern ContentStatus_CREATE_FAILED = ContentStatus' "CREATE_FAILED"

pattern ContentStatus_CREATE_IN_PROGRESS :: ContentStatus
pattern ContentStatus_CREATE_IN_PROGRESS = ContentStatus' "CREATE_IN_PROGRESS"

pattern ContentStatus_DELETED :: ContentStatus
pattern ContentStatus_DELETED = ContentStatus' "DELETED"

pattern ContentStatus_DELETE_FAILED :: ContentStatus
pattern ContentStatus_DELETE_FAILED = ContentStatus' "DELETE_FAILED"

pattern ContentStatus_DELETE_IN_PROGRESS :: ContentStatus
pattern ContentStatus_DELETE_IN_PROGRESS = ContentStatus' "DELETE_IN_PROGRESS"

pattern ContentStatus_UPDATE_FAILED :: ContentStatus
pattern ContentStatus_UPDATE_FAILED = ContentStatus' "UPDATE_FAILED"

{-# COMPLETE
  ContentStatus_ACTIVE,
  ContentStatus_CREATE_FAILED,
  ContentStatus_CREATE_IN_PROGRESS,
  ContentStatus_DELETED,
  ContentStatus_DELETE_FAILED,
  ContentStatus_DELETE_IN_PROGRESS,
  ContentStatus_UPDATE_FAILED,
  ContentStatus'
  #-}
