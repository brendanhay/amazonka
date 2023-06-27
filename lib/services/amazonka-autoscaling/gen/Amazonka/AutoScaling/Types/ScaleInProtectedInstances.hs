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
-- Module      : Amazonka.AutoScaling.Types.ScaleInProtectedInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.ScaleInProtectedInstances
  ( ScaleInProtectedInstances
      ( ..,
        ScaleInProtectedInstances_Ignore,
        ScaleInProtectedInstances_Refresh,
        ScaleInProtectedInstances_Wait
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScaleInProtectedInstances = ScaleInProtectedInstances'
  { fromScaleInProtectedInstances ::
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

pattern ScaleInProtectedInstances_Ignore :: ScaleInProtectedInstances
pattern ScaleInProtectedInstances_Ignore = ScaleInProtectedInstances' "Ignore"

pattern ScaleInProtectedInstances_Refresh :: ScaleInProtectedInstances
pattern ScaleInProtectedInstances_Refresh = ScaleInProtectedInstances' "Refresh"

pattern ScaleInProtectedInstances_Wait :: ScaleInProtectedInstances
pattern ScaleInProtectedInstances_Wait = ScaleInProtectedInstances' "Wait"

{-# COMPLETE
  ScaleInProtectedInstances_Ignore,
  ScaleInProtectedInstances_Refresh,
  ScaleInProtectedInstances_Wait,
  ScaleInProtectedInstances'
  #-}
