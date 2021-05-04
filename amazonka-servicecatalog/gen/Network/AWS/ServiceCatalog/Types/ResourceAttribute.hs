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

import qualified Network.AWS.Prelude as Prelude

newtype ResourceAttribute = ResourceAttribute'
  { fromResourceAttribute ::
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
