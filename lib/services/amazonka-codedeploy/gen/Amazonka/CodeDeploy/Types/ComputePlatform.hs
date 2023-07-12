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
-- Module      : Amazonka.CodeDeploy.Types.ComputePlatform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ComputePlatform
  ( ComputePlatform
      ( ..,
        ComputePlatform_ECS,
        ComputePlatform_Lambda,
        ComputePlatform_Server
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComputePlatform = ComputePlatform'
  { fromComputePlatform ::
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

pattern ComputePlatform_ECS :: ComputePlatform
pattern ComputePlatform_ECS = ComputePlatform' "ECS"

pattern ComputePlatform_Lambda :: ComputePlatform
pattern ComputePlatform_Lambda = ComputePlatform' "Lambda"

pattern ComputePlatform_Server :: ComputePlatform
pattern ComputePlatform_Server = ComputePlatform' "Server"

{-# COMPLETE
  ComputePlatform_ECS,
  ComputePlatform_Lambda,
  ComputePlatform_Server,
  ComputePlatform'
  #-}
