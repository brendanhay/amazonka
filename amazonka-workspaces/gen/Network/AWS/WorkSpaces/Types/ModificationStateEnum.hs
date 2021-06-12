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
-- Module      : Network.AWS.WorkSpaces.Types.ModificationStateEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationStateEnum
  ( ModificationStateEnum
      ( ..,
        ModificationStateEnum_UPDATE_INITIATED,
        ModificationStateEnum_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ModificationStateEnum = ModificationStateEnum'
  { fromModificationStateEnum ::
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

pattern ModificationStateEnum_UPDATE_INITIATED :: ModificationStateEnum
pattern ModificationStateEnum_UPDATE_INITIATED = ModificationStateEnum' "UPDATE_INITIATED"

pattern ModificationStateEnum_UPDATE_IN_PROGRESS :: ModificationStateEnum
pattern ModificationStateEnum_UPDATE_IN_PROGRESS = ModificationStateEnum' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  ModificationStateEnum_UPDATE_INITIATED,
  ModificationStateEnum_UPDATE_IN_PROGRESS,
  ModificationStateEnum'
  #-}
