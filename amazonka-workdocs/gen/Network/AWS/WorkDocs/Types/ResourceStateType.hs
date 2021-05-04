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
-- Module      : Network.AWS.WorkDocs.Types.ResourceStateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceStateType
  ( ResourceStateType
      ( ..,
        ResourceStateType_ACTIVE,
        ResourceStateType_RECYCLED,
        ResourceStateType_RECYCLING,
        ResourceStateType_RESTORING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ResourceStateType = ResourceStateType'
  { fromResourceStateType ::
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

pattern ResourceStateType_ACTIVE :: ResourceStateType
pattern ResourceStateType_ACTIVE = ResourceStateType' "ACTIVE"

pattern ResourceStateType_RECYCLED :: ResourceStateType
pattern ResourceStateType_RECYCLED = ResourceStateType' "RECYCLED"

pattern ResourceStateType_RECYCLING :: ResourceStateType
pattern ResourceStateType_RECYCLING = ResourceStateType' "RECYCLING"

pattern ResourceStateType_RESTORING :: ResourceStateType
pattern ResourceStateType_RESTORING = ResourceStateType' "RESTORING"

{-# COMPLETE
  ResourceStateType_ACTIVE,
  ResourceStateType_RECYCLED,
  ResourceStateType_RECYCLING,
  ResourceStateType_RESTORING,
  ResourceStateType'
  #-}
