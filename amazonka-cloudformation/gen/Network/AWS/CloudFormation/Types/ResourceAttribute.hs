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
-- Module      : Network.AWS.CloudFormation.Types.ResourceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceAttribute
  ( ResourceAttribute
      ( ..,
        ResourceAttribute_CreationPolicy,
        ResourceAttribute_DeletionPolicy,
        ResourceAttribute_Metadata,
        ResourceAttribute_Properties,
        ResourceAttribute_Tags,
        ResourceAttribute_UpdatePolicy
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceAttribute = ResourceAttribute'
  { fromResourceAttribute ::
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

pattern ResourceAttribute_CreationPolicy :: ResourceAttribute
pattern ResourceAttribute_CreationPolicy = ResourceAttribute' "CreationPolicy"

pattern ResourceAttribute_DeletionPolicy :: ResourceAttribute
pattern ResourceAttribute_DeletionPolicy = ResourceAttribute' "DeletionPolicy"

pattern ResourceAttribute_Metadata :: ResourceAttribute
pattern ResourceAttribute_Metadata = ResourceAttribute' "Metadata"

pattern ResourceAttribute_Properties :: ResourceAttribute
pattern ResourceAttribute_Properties = ResourceAttribute' "Properties"

pattern ResourceAttribute_Tags :: ResourceAttribute
pattern ResourceAttribute_Tags = ResourceAttribute' "Tags"

pattern ResourceAttribute_UpdatePolicy :: ResourceAttribute
pattern ResourceAttribute_UpdatePolicy = ResourceAttribute' "UpdatePolicy"

{-# COMPLETE
  ResourceAttribute_CreationPolicy,
  ResourceAttribute_DeletionPolicy,
  ResourceAttribute_Metadata,
  ResourceAttribute_Properties,
  ResourceAttribute_Tags,
  ResourceAttribute_UpdatePolicy,
  ResourceAttribute'
  #-}
