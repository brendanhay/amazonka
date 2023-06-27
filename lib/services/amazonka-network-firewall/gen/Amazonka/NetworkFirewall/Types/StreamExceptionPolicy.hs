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
-- Module      : Amazonka.NetworkFirewall.Types.StreamExceptionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StreamExceptionPolicy
  ( StreamExceptionPolicy
      ( ..,
        StreamExceptionPolicy_CONTINUE,
        StreamExceptionPolicy_DROP,
        StreamExceptionPolicy_REJECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamExceptionPolicy = StreamExceptionPolicy'
  { fromStreamExceptionPolicy ::
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

pattern StreamExceptionPolicy_CONTINUE :: StreamExceptionPolicy
pattern StreamExceptionPolicy_CONTINUE = StreamExceptionPolicy' "CONTINUE"

pattern StreamExceptionPolicy_DROP :: StreamExceptionPolicy
pattern StreamExceptionPolicy_DROP = StreamExceptionPolicy' "DROP"

pattern StreamExceptionPolicy_REJECT :: StreamExceptionPolicy
pattern StreamExceptionPolicy_REJECT = StreamExceptionPolicy' "REJECT"

{-# COMPLETE
  StreamExceptionPolicy_CONTINUE,
  StreamExceptionPolicy_DROP,
  StreamExceptionPolicy_REJECT,
  StreamExceptionPolicy'
  #-}
