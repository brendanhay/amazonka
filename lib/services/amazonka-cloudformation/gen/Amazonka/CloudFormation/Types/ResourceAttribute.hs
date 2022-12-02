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
-- Module      : Amazonka.CloudFormation.Types.ResourceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ResourceAttribute
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceAttribute = ResourceAttribute'
  { fromResourceAttribute ::
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
