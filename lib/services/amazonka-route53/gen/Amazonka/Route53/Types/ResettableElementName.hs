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
-- Module      : Amazonka.Route53.Types.ResettableElementName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.ResettableElementName
  ( ResettableElementName
      ( ..,
        ResettableElementName_ChildHealthChecks,
        ResettableElementName_FullyQualifiedDomainName,
        ResettableElementName_Regions,
        ResettableElementName_ResourcePath
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype ResettableElementName = ResettableElementName'
  { fromResettableElementName ::
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
