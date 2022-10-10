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
-- Module      : Amazonka.ServiceCatalog.Types.ResourceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ResourceAttribute
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

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
