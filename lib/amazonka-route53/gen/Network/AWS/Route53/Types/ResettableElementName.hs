{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResettableElementName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResettableElementName
  ( ResettableElementName
      ( ResettableElementName',
        FullyQualifiedDomainName,
        Regions,
        ResourcePath,
        ChildHealthChecks
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype ResettableElementName = ResettableElementName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern FullyQualifiedDomainName :: ResettableElementName
pattern FullyQualifiedDomainName = ResettableElementName' "FullyQualifiedDomainName"

pattern Regions :: ResettableElementName
pattern Regions = ResettableElementName' "Regions"

pattern ResourcePath :: ResettableElementName
pattern ResourcePath = ResettableElementName' "ResourcePath"

pattern ChildHealthChecks :: ResettableElementName
pattern ChildHealthChecks = ResettableElementName' "ChildHealthChecks"

{-# COMPLETE
  FullyQualifiedDomainName,
  Regions,
  ResourcePath,
  ChildHealthChecks,
  ResettableElementName'
  #-}
