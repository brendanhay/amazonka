{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceAttribute
  ( ResourceAttribute
      ( ResourceAttribute',
        Properties,
        Metadata,
        CreationPolicy,
        UpdatePolicy,
        DeletionPolicy,
        Tags
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceAttribute = ResourceAttribute' Lude.Text
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

pattern Properties :: ResourceAttribute
pattern Properties = ResourceAttribute' "Properties"

pattern Metadata :: ResourceAttribute
pattern Metadata = ResourceAttribute' "Metadata"

pattern CreationPolicy :: ResourceAttribute
pattern CreationPolicy = ResourceAttribute' "CreationPolicy"

pattern UpdatePolicy :: ResourceAttribute
pattern UpdatePolicy = ResourceAttribute' "UpdatePolicy"

pattern DeletionPolicy :: ResourceAttribute
pattern DeletionPolicy = ResourceAttribute' "DeletionPolicy"

pattern Tags :: ResourceAttribute
pattern Tags = ResourceAttribute' "Tags"

{-# COMPLETE
  Properties,
  Metadata,
  CreationPolicy,
  UpdatePolicy,
  DeletionPolicy,
  Tags,
  ResourceAttribute'
  #-}
