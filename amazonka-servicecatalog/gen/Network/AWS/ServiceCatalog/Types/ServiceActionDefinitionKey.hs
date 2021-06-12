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
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
  ( ServiceActionDefinitionKey
      ( ..,
        ServiceActionDefinitionKey_AssumeRole,
        ServiceActionDefinitionKey_Name,
        ServiceActionDefinitionKey_Parameters,
        ServiceActionDefinitionKey_Version
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ServiceActionDefinitionKey = ServiceActionDefinitionKey'
  { fromServiceActionDefinitionKey ::
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
