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
-- Module      : Amazonka.Proton.Types.ProvisionedResourceEngine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ProvisionedResourceEngine
  ( ProvisionedResourceEngine
      ( ..,
        ProvisionedResourceEngine_CLOUDFORMATION,
        ProvisionedResourceEngine_TERRAFORM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of provisioning engines
newtype ProvisionedResourceEngine = ProvisionedResourceEngine'
  { fromProvisionedResourceEngine ::
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

pattern ProvisionedResourceEngine_CLOUDFORMATION :: ProvisionedResourceEngine
pattern ProvisionedResourceEngine_CLOUDFORMATION = ProvisionedResourceEngine' "CLOUDFORMATION"

pattern ProvisionedResourceEngine_TERRAFORM :: ProvisionedResourceEngine
pattern ProvisionedResourceEngine_TERRAFORM = ProvisionedResourceEngine' "TERRAFORM"

{-# COMPLETE
  ProvisionedResourceEngine_CLOUDFORMATION,
  ProvisionedResourceEngine_TERRAFORM,
  ProvisionedResourceEngine'
  #-}
