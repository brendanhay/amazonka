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
-- Module      : Amazonka.MGN.Types.FirstBoot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.FirstBoot
  ( FirstBoot
      ( ..,
        FirstBoot_STOPPED,
        FirstBoot_SUCCEEDED,
        FirstBoot_UNKNOWN,
        FirstBoot_WAITING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FirstBoot = FirstBoot'
  { fromFirstBoot ::
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

pattern FirstBoot_STOPPED :: FirstBoot
pattern FirstBoot_STOPPED = FirstBoot' "STOPPED"

pattern FirstBoot_SUCCEEDED :: FirstBoot
pattern FirstBoot_SUCCEEDED = FirstBoot' "SUCCEEDED"

pattern FirstBoot_UNKNOWN :: FirstBoot
pattern FirstBoot_UNKNOWN = FirstBoot' "UNKNOWN"

pattern FirstBoot_WAITING :: FirstBoot
pattern FirstBoot_WAITING = FirstBoot' "WAITING"

{-# COMPLETE
  FirstBoot_STOPPED,
  FirstBoot_SUCCEEDED,
  FirstBoot_UNKNOWN,
  FirstBoot_WAITING,
  FirstBoot'
  #-}
