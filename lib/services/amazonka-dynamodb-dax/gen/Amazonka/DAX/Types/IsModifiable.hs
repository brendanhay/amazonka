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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype IsModifiable = IsModifiable'
  { fromIsModifiable ::
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
