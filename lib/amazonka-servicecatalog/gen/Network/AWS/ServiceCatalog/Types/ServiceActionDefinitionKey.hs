{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
  ( ServiceActionDefinitionKey
      ( ServiceActionDefinitionKey',
        AssumeRole,
        Name,
        Parameters,
        Version
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServiceActionDefinitionKey = ServiceActionDefinitionKey' Lude.Text
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

pattern AssumeRole :: ServiceActionDefinitionKey
pattern AssumeRole = ServiceActionDefinitionKey' "AssumeRole"

pattern Name :: ServiceActionDefinitionKey
pattern Name = ServiceActionDefinitionKey' "Name"

pattern Parameters :: ServiceActionDefinitionKey
pattern Parameters = ServiceActionDefinitionKey' "Parameters"

pattern Version :: ServiceActionDefinitionKey
pattern Version = ServiceActionDefinitionKey' "Version"

{-# COMPLETE
  AssumeRole,
  Name,
  Parameters,
  Version,
  ServiceActionDefinitionKey'
  #-}
