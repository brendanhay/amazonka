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
-- Module      : Amazonka.Inspector2.Types.EcrRescanDuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrRescanDuration
  ( EcrRescanDuration
      ( ..,
        EcrRescanDuration_DAYS_180,
        EcrRescanDuration_DAYS_30,
        EcrRescanDuration_LIFETIME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EcrRescanDuration = EcrRescanDuration'
  { fromEcrRescanDuration ::
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

pattern EcrRescanDuration_DAYS_180 :: EcrRescanDuration
pattern EcrRescanDuration_DAYS_180 = EcrRescanDuration' "DAYS_180"

pattern EcrRescanDuration_DAYS_30 :: EcrRescanDuration
pattern EcrRescanDuration_DAYS_30 = EcrRescanDuration' "DAYS_30"

pattern EcrRescanDuration_LIFETIME :: EcrRescanDuration
pattern EcrRescanDuration_LIFETIME = EcrRescanDuration' "LIFETIME"

{-# COMPLETE
  EcrRescanDuration_DAYS_180,
  EcrRescanDuration_DAYS_30,
  EcrRescanDuration_LIFETIME,
  EcrRescanDuration'
  #-}
