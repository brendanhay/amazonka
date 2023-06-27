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
-- Module      : Amazonka.FraudDetector.Types.ListUpdateMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ListUpdateMode
  ( ListUpdateMode
      ( ..,
        ListUpdateMode_APPEND,
        ListUpdateMode_REMOVE,
        ListUpdateMode_REPLACE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListUpdateMode = ListUpdateMode'
  { fromListUpdateMode ::
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

pattern ListUpdateMode_APPEND :: ListUpdateMode
pattern ListUpdateMode_APPEND = ListUpdateMode' "APPEND"

pattern ListUpdateMode_REMOVE :: ListUpdateMode
pattern ListUpdateMode_REMOVE = ListUpdateMode' "REMOVE"

pattern ListUpdateMode_REPLACE :: ListUpdateMode
pattern ListUpdateMode_REPLACE = ListUpdateMode' "REPLACE"

{-# COMPLETE
  ListUpdateMode_APPEND,
  ListUpdateMode_REMOVE,
  ListUpdateMode_REPLACE,
  ListUpdateMode'
  #-}
