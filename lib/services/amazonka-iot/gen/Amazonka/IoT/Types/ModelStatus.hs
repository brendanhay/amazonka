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
-- Module      : Amazonka.IoT.Types.ModelStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_ACTIVE,
        ModelStatus_EXPIRED,
        ModelStatus_PENDING_BUILD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
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

pattern ModelStatus_ACTIVE :: ModelStatus
pattern ModelStatus_ACTIVE = ModelStatus' "ACTIVE"

pattern ModelStatus_EXPIRED :: ModelStatus
pattern ModelStatus_EXPIRED = ModelStatus' "EXPIRED"

pattern ModelStatus_PENDING_BUILD :: ModelStatus
pattern ModelStatus_PENDING_BUILD = ModelStatus' "PENDING_BUILD"

{-# COMPLETE
  ModelStatus_ACTIVE,
  ModelStatus_EXPIRED,
  ModelStatus_PENDING_BUILD,
  ModelStatus'
  #-}
