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
        CreationPolicy,
        DeletionPolicy,
        Metadata,
        Properties,
        Tags,
        UpdatePolicy
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

pattern CreationPolicy :: ResourceAttribute
pattern CreationPolicy = ResourceAttribute' "CreationPolicy"

pattern DeletionPolicy :: ResourceAttribute
pattern DeletionPolicy = ResourceAttribute' "DeletionPolicy"

pattern Metadata :: ResourceAttribute
pattern Metadata = ResourceAttribute' "Metadata"

pattern Properties :: ResourceAttribute
pattern Properties = ResourceAttribute' "Properties"

pattern Tags :: ResourceAttribute
pattern Tags = ResourceAttribute' "Tags"

pattern UpdatePolicy :: ResourceAttribute
pattern UpdatePolicy = ResourceAttribute' "UpdatePolicy"

{-# COMPLETE
  CreationPolicy,
  DeletionPolicy,
  Metadata,
  Properties,
  Tags,
  UpdatePolicy,
  ResourceAttribute'
  #-}
