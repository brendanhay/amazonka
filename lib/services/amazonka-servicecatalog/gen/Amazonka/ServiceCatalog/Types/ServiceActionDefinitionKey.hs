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
-- Module      : Amazonka.ServiceCatalog.Types.ServiceActionDefinitionKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ServiceActionDefinitionKey
  ( ServiceActionDefinitionKey
      ( ..,
        ServiceActionDefinitionKey_AssumeRole,
        ServiceActionDefinitionKey_Name,
        ServiceActionDefinitionKey_Parameters,
        ServiceActionDefinitionKey_Version
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceActionDefinitionKey = ServiceActionDefinitionKey'
  { fromServiceActionDefinitionKey ::
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

pattern ServiceActionDefinitionKey_AssumeRole :: ServiceActionDefinitionKey
pattern ServiceActionDefinitionKey_AssumeRole = ServiceActionDefinitionKey' "AssumeRole"

pattern ServiceActionDefinitionKey_Name :: ServiceActionDefinitionKey
pattern ServiceActionDefinitionKey_Name = ServiceActionDefinitionKey' "Name"

pattern ServiceActionDefinitionKey_Parameters :: ServiceActionDefinitionKey
pattern ServiceActionDefinitionKey_Parameters = ServiceActionDefinitionKey' "Parameters"

pattern ServiceActionDefinitionKey_Version :: ServiceActionDefinitionKey
pattern ServiceActionDefinitionKey_Version = ServiceActionDefinitionKey' "Version"

{-# COMPLETE
  ServiceActionDefinitionKey_AssumeRole,
  ServiceActionDefinitionKey_Name,
  ServiceActionDefinitionKey_Parameters,
  ServiceActionDefinitionKey_Version,
  ServiceActionDefinitionKey'
  #-}
