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
-- Module      : Network.AWS.Glue.Types.EnableHybridValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EnableHybridValues
  ( EnableHybridValues
      ( ..,
        EnableHybridValues_FALSE,
        EnableHybridValues_TRUE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EnableHybridValues = EnableHybridValues'
  { fromEnableHybridValues ::
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

pattern EnableHybridValues_FALSE :: EnableHybridValues
pattern EnableHybridValues_FALSE = EnableHybridValues' "FALSE"

pattern EnableHybridValues_TRUE :: EnableHybridValues
pattern EnableHybridValues_TRUE = EnableHybridValues' "TRUE"

{-# COMPLETE
  EnableHybridValues_FALSE,
  EnableHybridValues_TRUE,
  EnableHybridValues'
  #-}
