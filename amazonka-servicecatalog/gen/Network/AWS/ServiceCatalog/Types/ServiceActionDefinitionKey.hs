{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ServiceActionDefinitionKey = ServiceActionDefinitionKey'
  { fromServiceActionDefinitionKey ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
