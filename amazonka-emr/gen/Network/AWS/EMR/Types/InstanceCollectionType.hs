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
-- Module      : Network.AWS.EMR.Types.InstanceCollectionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceCollectionType
  ( InstanceCollectionType
      ( ..,
        InstanceCollectionType_INSTANCE_FLEET,
        InstanceCollectionType_INSTANCE_GROUP
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstanceCollectionType = InstanceCollectionType'
  { fromInstanceCollectionType ::
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

pattern InstanceCollectionType_INSTANCE_FLEET :: InstanceCollectionType
pattern InstanceCollectionType_INSTANCE_FLEET = InstanceCollectionType' "INSTANCE_FLEET"

pattern InstanceCollectionType_INSTANCE_GROUP :: InstanceCollectionType
pattern InstanceCollectionType_INSTANCE_GROUP = InstanceCollectionType' "INSTANCE_GROUP"

{-# COMPLETE
  InstanceCollectionType_INSTANCE_FLEET,
  InstanceCollectionType_INSTANCE_GROUP,
  InstanceCollectionType'
  #-}
