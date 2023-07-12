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
-- Module      : Amazonka.ECS.Types.ExecuteCommandLogging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ExecuteCommandLogging
  ( ExecuteCommandLogging
      ( ..,
        ExecuteCommandLogging_DEFAULT,
        ExecuteCommandLogging_NONE,
        ExecuteCommandLogging_OVERRIDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecuteCommandLogging = ExecuteCommandLogging'
  { fromExecuteCommandLogging ::
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

pattern ExecuteCommandLogging_DEFAULT :: ExecuteCommandLogging
pattern ExecuteCommandLogging_DEFAULT = ExecuteCommandLogging' "DEFAULT"

pattern ExecuteCommandLogging_NONE :: ExecuteCommandLogging
pattern ExecuteCommandLogging_NONE = ExecuteCommandLogging' "NONE"

pattern ExecuteCommandLogging_OVERRIDE :: ExecuteCommandLogging
pattern ExecuteCommandLogging_OVERRIDE = ExecuteCommandLogging' "OVERRIDE"

{-# COMPLETE
  ExecuteCommandLogging_DEFAULT,
  ExecuteCommandLogging_NONE,
  ExecuteCommandLogging_OVERRIDE,
  ExecuteCommandLogging'
  #-}
