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
-- Module      : Amazonka.Kendra.Types.ThesaurusStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ThesaurusStatus
  ( ThesaurusStatus
      ( ..,
        ThesaurusStatus_ACTIVE,
        ThesaurusStatus_ACTIVE_BUT_UPDATE_FAILED,
        ThesaurusStatus_CREATING,
        ThesaurusStatus_DELETING,
        ThesaurusStatus_FAILED,
        ThesaurusStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ThesaurusStatus = ThesaurusStatus'
  { fromThesaurusStatus ::
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

pattern ThesaurusStatus_ACTIVE :: ThesaurusStatus
pattern ThesaurusStatus_ACTIVE = ThesaurusStatus' "ACTIVE"

pattern ThesaurusStatus_ACTIVE_BUT_UPDATE_FAILED :: ThesaurusStatus
pattern ThesaurusStatus_ACTIVE_BUT_UPDATE_FAILED = ThesaurusStatus' "ACTIVE_BUT_UPDATE_FAILED"

pattern ThesaurusStatus_CREATING :: ThesaurusStatus
pattern ThesaurusStatus_CREATING = ThesaurusStatus' "CREATING"

pattern ThesaurusStatus_DELETING :: ThesaurusStatus
pattern ThesaurusStatus_DELETING = ThesaurusStatus' "DELETING"

pattern ThesaurusStatus_FAILED :: ThesaurusStatus
pattern ThesaurusStatus_FAILED = ThesaurusStatus' "FAILED"

pattern ThesaurusStatus_UPDATING :: ThesaurusStatus
pattern ThesaurusStatus_UPDATING = ThesaurusStatus' "UPDATING"

{-# COMPLETE
  ThesaurusStatus_ACTIVE,
  ThesaurusStatus_ACTIVE_BUT_UPDATE_FAILED,
  ThesaurusStatus_CREATING,
  ThesaurusStatus_DELETING,
  ThesaurusStatus_FAILED,
  ThesaurusStatus_UPDATING,
  ThesaurusStatus'
  #-}
