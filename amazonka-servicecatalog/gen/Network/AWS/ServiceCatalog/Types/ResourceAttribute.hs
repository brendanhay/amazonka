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
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceAttribute
  ( ResourceAttribute
      ( ..,
        ResourceAttribute_CREATIONPOLICY,
        ResourceAttribute_DELETIONPOLICY,
        ResourceAttribute_METADATA,
        ResourceAttribute_PROPERTIES,
        ResourceAttribute_TAGS,
        ResourceAttribute_UPDATEPOLICY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ResourceAttribute = ResourceAttribute'
  { fromResourceAttribute ::
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

pattern ResourceAttribute_CREATIONPOLICY :: ResourceAttribute
pattern ResourceAttribute_CREATIONPOLICY = ResourceAttribute' "CREATIONPOLICY"

pattern ResourceAttribute_DELETIONPOLICY :: ResourceAttribute
pattern ResourceAttribute_DELETIONPOLICY = ResourceAttribute' "DELETIONPOLICY"

pattern ResourceAttribute_METADATA :: ResourceAttribute
pattern ResourceAttribute_METADATA = ResourceAttribute' "METADATA"

pattern ResourceAttribute_PROPERTIES :: ResourceAttribute
pattern ResourceAttribute_PROPERTIES = ResourceAttribute' "PROPERTIES"

pattern ResourceAttribute_TAGS :: ResourceAttribute
pattern ResourceAttribute_TAGS = ResourceAttribute' "TAGS"

pattern ResourceAttribute_UPDATEPOLICY :: ResourceAttribute
pattern ResourceAttribute_UPDATEPOLICY = ResourceAttribute' "UPDATEPOLICY"

{-# COMPLETE
  ResourceAttribute_CREATIONPOLICY,
  ResourceAttribute_DELETIONPOLICY,
  ResourceAttribute_METADATA,
  ResourceAttribute_PROPERTIES,
  ResourceAttribute_TAGS,
  ResourceAttribute_UPDATEPOLICY,
  ResourceAttribute'
  #-}
