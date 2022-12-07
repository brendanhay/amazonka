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
-- Module      : Amazonka.MacieV2.Types.FindingsFilterAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingsFilterAction
  ( FindingsFilterAction
      ( ..,
        FindingsFilterAction_ARCHIVE,
        FindingsFilterAction_NOOP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action to perform on findings that meet the filter criteria. To
-- suppress (automatically archive) findings that meet the criteria, set
-- this value to ARCHIVE. Valid values are:
newtype FindingsFilterAction = FindingsFilterAction'
  { fromFindingsFilterAction ::
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

pattern FindingsFilterAction_ARCHIVE :: FindingsFilterAction
pattern FindingsFilterAction_ARCHIVE = FindingsFilterAction' "ARCHIVE"

pattern FindingsFilterAction_NOOP :: FindingsFilterAction
pattern FindingsFilterAction_NOOP = FindingsFilterAction' "NOOP"

{-# COMPLETE
  FindingsFilterAction_ARCHIVE,
  FindingsFilterAction_NOOP,
  FindingsFilterAction'
  #-}
