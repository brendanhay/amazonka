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
-- Module      : Amazonka.DAX.Types.IsModifiable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.IsModifiable
  ( IsModifiable
      ( ..,
        IsModifiable_CONDITIONAL,
        IsModifiable_FALSE,
        IsModifiable_TRUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IsModifiable = IsModifiable'
  { fromIsModifiable ::
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

pattern IsModifiable_CONDITIONAL :: IsModifiable
pattern IsModifiable_CONDITIONAL = IsModifiable' "CONDITIONAL"

pattern IsModifiable_FALSE :: IsModifiable
pattern IsModifiable_FALSE = IsModifiable' "FALSE"

pattern IsModifiable_TRUE :: IsModifiable
pattern IsModifiable_TRUE = IsModifiable' "TRUE"

{-# COMPLETE
  IsModifiable_CONDITIONAL,
  IsModifiable_FALSE,
  IsModifiable_TRUE,
  IsModifiable'
  #-}
