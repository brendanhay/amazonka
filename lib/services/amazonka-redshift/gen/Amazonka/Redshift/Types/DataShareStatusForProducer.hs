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
-- Module      : Amazonka.Redshift.Types.DataShareStatusForProducer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DataShareStatusForProducer
  ( DataShareStatusForProducer
      ( ..,
        DataShareStatusForProducer_ACTIVE,
        DataShareStatusForProducer_AUTHORIZED,
        DataShareStatusForProducer_DEAUTHORIZED,
        DataShareStatusForProducer_PENDING_AUTHORIZATION,
        DataShareStatusForProducer_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype DataShareStatusForProducer = DataShareStatusForProducer'
  { fromDataShareStatusForProducer ::
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

pattern DataShareStatusForProducer_ACTIVE :: DataShareStatusForProducer
pattern DataShareStatusForProducer_ACTIVE = DataShareStatusForProducer' "ACTIVE"

pattern DataShareStatusForProducer_AUTHORIZED :: DataShareStatusForProducer
pattern DataShareStatusForProducer_AUTHORIZED = DataShareStatusForProducer' "AUTHORIZED"

pattern DataShareStatusForProducer_DEAUTHORIZED :: DataShareStatusForProducer
pattern DataShareStatusForProducer_DEAUTHORIZED = DataShareStatusForProducer' "DEAUTHORIZED"

pattern DataShareStatusForProducer_PENDING_AUTHORIZATION :: DataShareStatusForProducer
pattern DataShareStatusForProducer_PENDING_AUTHORIZATION = DataShareStatusForProducer' "PENDING_AUTHORIZATION"

pattern DataShareStatusForProducer_REJECTED :: DataShareStatusForProducer
pattern DataShareStatusForProducer_REJECTED = DataShareStatusForProducer' "REJECTED"

{-# COMPLETE
  DataShareStatusForProducer_ACTIVE,
  DataShareStatusForProducer_AUTHORIZED,
  DataShareStatusForProducer_DEAUTHORIZED,
  DataShareStatusForProducer_PENDING_AUTHORIZATION,
  DataShareStatusForProducer_REJECTED,
  DataShareStatusForProducer'
  #-}
