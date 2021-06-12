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
-- Module      : Network.AWS.WorkSpaces.Types.ModificationResourceEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationResourceEnum
  ( ModificationResourceEnum
      ( ..,
        ModificationResourceEnum_COMPUTE_TYPE,
        ModificationResourceEnum_ROOT_VOLUME,
        ModificationResourceEnum_USER_VOLUME
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ModificationResourceEnum = ModificationResourceEnum'
  { fromModificationResourceEnum ::
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

pattern ModificationResourceEnum_COMPUTE_TYPE :: ModificationResourceEnum
pattern ModificationResourceEnum_COMPUTE_TYPE = ModificationResourceEnum' "COMPUTE_TYPE"

pattern ModificationResourceEnum_ROOT_VOLUME :: ModificationResourceEnum
pattern ModificationResourceEnum_ROOT_VOLUME = ModificationResourceEnum' "ROOT_VOLUME"

pattern ModificationResourceEnum_USER_VOLUME :: ModificationResourceEnum
pattern ModificationResourceEnum_USER_VOLUME = ModificationResourceEnum' "USER_VOLUME"

{-# COMPLETE
  ModificationResourceEnum_COMPUTE_TYPE,
  ModificationResourceEnum_ROOT_VOLUME,
  ModificationResourceEnum_USER_VOLUME,
  ModificationResourceEnum'
  #-}
