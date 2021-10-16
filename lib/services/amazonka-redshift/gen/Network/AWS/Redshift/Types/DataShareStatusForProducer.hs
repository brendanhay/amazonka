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
-- Module      : Network.AWS.Redshift.Types.DataShareStatusForProducer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DataShareStatusForProducer
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype DataShareStatusForProducer = DataShareStatusForProducer'
  { fromDataShareStatusForProducer ::
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
