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
-- Module      : Amazonka.DMS.Types.PluginNameValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.PluginNameValue
  ( PluginNameValue
      ( ..,
        PluginNameValue_No_preference,
        PluginNameValue_Pglogical,
        PluginNameValue_Test_decoding
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PluginNameValue = PluginNameValue'
  { fromPluginNameValue ::
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

pattern PluginNameValue_No_preference :: PluginNameValue
pattern PluginNameValue_No_preference = PluginNameValue' "no-preference"

pattern PluginNameValue_Pglogical :: PluginNameValue
pattern PluginNameValue_Pglogical = PluginNameValue' "pglogical"

pattern PluginNameValue_Test_decoding :: PluginNameValue
pattern PluginNameValue_Test_decoding = PluginNameValue' "test-decoding"

{-# COMPLETE
  PluginNameValue_No_preference,
  PluginNameValue_Pglogical,
  PluginNameValue_Test_decoding,
  PluginNameValue'
  #-}
