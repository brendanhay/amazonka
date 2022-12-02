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
-- Module      : Amazonka.SWF.Types.ChildPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ChildPolicy
  ( ChildPolicy
      ( ..,
        ChildPolicy_ABANDON,
        ChildPolicy_REQUEST_CANCEL,
        ChildPolicy_TERMINATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChildPolicy = ChildPolicy'
  { fromChildPolicy ::
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

pattern ChildPolicy_ABANDON :: ChildPolicy
pattern ChildPolicy_ABANDON = ChildPolicy' "ABANDON"

pattern ChildPolicy_REQUEST_CANCEL :: ChildPolicy
pattern ChildPolicy_REQUEST_CANCEL = ChildPolicy' "REQUEST_CANCEL"

pattern ChildPolicy_TERMINATE :: ChildPolicy
pattern ChildPolicy_TERMINATE = ChildPolicy' "TERMINATE"

{-# COMPLETE
  ChildPolicy_ABANDON,
  ChildPolicy_REQUEST_CANCEL,
  ChildPolicy_TERMINATE,
  ChildPolicy'
  #-}
