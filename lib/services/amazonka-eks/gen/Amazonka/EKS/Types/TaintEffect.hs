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
-- Module      : Amazonka.EKS.Types.TaintEffect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.TaintEffect
  ( TaintEffect
      ( ..,
        TaintEffect_NO_EXECUTE,
        TaintEffect_NO_SCHEDULE,
        TaintEffect_PREFER_NO_SCHEDULE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaintEffect = TaintEffect'
  { fromTaintEffect ::
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

pattern TaintEffect_NO_EXECUTE :: TaintEffect
pattern TaintEffect_NO_EXECUTE = TaintEffect' "NO_EXECUTE"

pattern TaintEffect_NO_SCHEDULE :: TaintEffect
pattern TaintEffect_NO_SCHEDULE = TaintEffect' "NO_SCHEDULE"

pattern TaintEffect_PREFER_NO_SCHEDULE :: TaintEffect
pattern TaintEffect_PREFER_NO_SCHEDULE = TaintEffect' "PREFER_NO_SCHEDULE"

{-# COMPLETE
  TaintEffect_NO_EXECUTE,
  TaintEffect_NO_SCHEDULE,
  TaintEffect_PREFER_NO_SCHEDULE,
  TaintEffect'
  #-}
