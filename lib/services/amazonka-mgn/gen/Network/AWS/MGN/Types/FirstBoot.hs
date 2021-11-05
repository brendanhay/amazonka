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
-- Module      : Network.AWS.MGN.Types.FirstBoot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.FirstBoot
  ( FirstBoot
      ( ..,
        FirstBoot_STOPPED,
        FirstBoot_SUCCEEDED,
        FirstBoot_UNKNOWN,
        FirstBoot_WAITING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FirstBoot = FirstBoot'
  { fromFirstBoot ::
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
