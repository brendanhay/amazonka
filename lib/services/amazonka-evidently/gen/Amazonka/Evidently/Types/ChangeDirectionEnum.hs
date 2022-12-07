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
-- Module      : Amazonka.Evidently.Types.ChangeDirectionEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ChangeDirectionEnum
  ( ChangeDirectionEnum
      ( ..,
        ChangeDirectionEnum_DECREASE,
        ChangeDirectionEnum_INCREASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeDirectionEnum = ChangeDirectionEnum'
  { fromChangeDirectionEnum ::
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

pattern ChangeDirectionEnum_DECREASE :: ChangeDirectionEnum
pattern ChangeDirectionEnum_DECREASE = ChangeDirectionEnum' "DECREASE"

pattern ChangeDirectionEnum_INCREASE :: ChangeDirectionEnum
pattern ChangeDirectionEnum_INCREASE = ChangeDirectionEnum' "INCREASE"

{-# COMPLETE
  ChangeDirectionEnum_DECREASE,
  ChangeDirectionEnum_INCREASE,
  ChangeDirectionEnum'
  #-}
