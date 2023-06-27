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
-- Module      : Amazonka.LexV2Models.Types.BotFilterOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotFilterOperator
  ( BotFilterOperator
      ( ..,
        BotFilterOperator_CO,
        BotFilterOperator_EQ,
        BotFilterOperator_NE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BotFilterOperator = BotFilterOperator'
  { fromBotFilterOperator ::
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

pattern BotFilterOperator_CO :: BotFilterOperator
pattern BotFilterOperator_CO = BotFilterOperator' "CO"

pattern BotFilterOperator_EQ :: BotFilterOperator
pattern BotFilterOperator_EQ = BotFilterOperator' "EQ"

pattern BotFilterOperator_NE :: BotFilterOperator
pattern BotFilterOperator_NE = BotFilterOperator' "NE"

{-# COMPLETE
  BotFilterOperator_CO,
  BotFilterOperator_EQ,
  BotFilterOperator_NE,
  BotFilterOperator'
  #-}
