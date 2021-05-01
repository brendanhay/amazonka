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
-- Module      : Network.AWS.Route53.Types.ResettableElementName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResettableElementName
  ( ResettableElementName
      ( ..,
        ResettableElementName_ChildHealthChecks,
        ResettableElementName_FullyQualifiedDomainName,
        ResettableElementName_Regions,
        ResettableElementName_ResourcePath
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype ResettableElementName = ResettableElementName'
  { fromResettableElementName ::
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

pattern ResettableElementName_ChildHealthChecks :: ResettableElementName
pattern ResettableElementName_ChildHealthChecks = ResettableElementName' "ChildHealthChecks"

pattern ResettableElementName_FullyQualifiedDomainName :: ResettableElementName
pattern ResettableElementName_FullyQualifiedDomainName = ResettableElementName' "FullyQualifiedDomainName"

pattern ResettableElementName_Regions :: ResettableElementName
pattern ResettableElementName_Regions = ResettableElementName' "Regions"

pattern ResettableElementName_ResourcePath :: ResettableElementName
pattern ResettableElementName_ResourcePath = ResettableElementName' "ResourcePath"

{-# COMPLETE
  ResettableElementName_ChildHealthChecks,
  ResettableElementName_FullyQualifiedDomainName,
  ResettableElementName_Regions,
  ResettableElementName_ResourcePath,
  ResettableElementName'
  #-}
