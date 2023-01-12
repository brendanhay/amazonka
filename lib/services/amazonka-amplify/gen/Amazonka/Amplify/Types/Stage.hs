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
-- Module      : Amazonka.Amplify.Types.Stage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Stage
  ( Stage
      ( ..,
        Stage_BETA,
        Stage_DEVELOPMENT,
        Stage_EXPERIMENTAL,
        Stage_PRODUCTION,
        Stage_PULL_REQUEST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Stage = Stage' {fromStage :: Data.Text}
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

pattern Stage_BETA :: Stage
pattern Stage_BETA = Stage' "BETA"

pattern Stage_DEVELOPMENT :: Stage
pattern Stage_DEVELOPMENT = Stage' "DEVELOPMENT"

pattern Stage_EXPERIMENTAL :: Stage
pattern Stage_EXPERIMENTAL = Stage' "EXPERIMENTAL"

pattern Stage_PRODUCTION :: Stage
pattern Stage_PRODUCTION = Stage' "PRODUCTION"

pattern Stage_PULL_REQUEST :: Stage
pattern Stage_PULL_REQUEST = Stage' "PULL_REQUEST"

{-# COMPLETE
  Stage_BETA,
  Stage_DEVELOPMENT,
  Stage_EXPERIMENTAL,
  Stage_PRODUCTION,
  Stage_PULL_REQUEST,
  Stage'
  #-}
