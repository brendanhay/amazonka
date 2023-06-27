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
-- Module      : Amazonka.AutoScaling.Types.StandbyInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.StandbyInstances
  ( StandbyInstances
      ( ..,
        StandbyInstances_Ignore,
        StandbyInstances_Terminate,
        StandbyInstances_Wait
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StandbyInstances = StandbyInstances'
  { fromStandbyInstances ::
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

pattern StandbyInstances_Ignore :: StandbyInstances
pattern StandbyInstances_Ignore = StandbyInstances' "Ignore"

pattern StandbyInstances_Terminate :: StandbyInstances
pattern StandbyInstances_Terminate = StandbyInstances' "Terminate"

pattern StandbyInstances_Wait :: StandbyInstances
pattern StandbyInstances_Wait = StandbyInstances' "Wait"

{-# COMPLETE
  StandbyInstances_Ignore,
  StandbyInstances_Terminate,
  StandbyInstances_Wait,
  StandbyInstances'
  #-}
