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
-- Module      : Network.AWS.DAX.Types.IsModifiable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.IsModifiable
  ( IsModifiable
      ( ..,
        IsModifiable_CONDITIONAL,
        IsModifiable_FALSE,
        IsModifiable_TRUE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype IsModifiable = IsModifiable'
  { fromIsModifiable ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
