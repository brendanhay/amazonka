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
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingStatusType
  ( ScalingStatusType
      ( ..,
        ScalingStatusType_ACTIVE,
        ScalingStatusType_DELETED,
        ScalingStatusType_DELETE_REQUESTED,
        ScalingStatusType_DELETING,
        ScalingStatusType_ERROR,
        ScalingStatusType_UPDATE_REQUESTED,
        ScalingStatusType_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ScalingStatusType = ScalingStatusType'
  { fromScalingStatusType ::
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

pattern ScalingStatusType_ACTIVE :: ScalingStatusType
pattern ScalingStatusType_ACTIVE = ScalingStatusType' "ACTIVE"

pattern ScalingStatusType_DELETED :: ScalingStatusType
pattern ScalingStatusType_DELETED = ScalingStatusType' "DELETED"

pattern ScalingStatusType_DELETE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_DELETE_REQUESTED = ScalingStatusType' "DELETE_REQUESTED"

pattern ScalingStatusType_DELETING :: ScalingStatusType
pattern ScalingStatusType_DELETING = ScalingStatusType' "DELETING"

pattern ScalingStatusType_ERROR :: ScalingStatusType
pattern ScalingStatusType_ERROR = ScalingStatusType' "ERROR"

pattern ScalingStatusType_UPDATE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_UPDATE_REQUESTED = ScalingStatusType' "UPDATE_REQUESTED"

pattern ScalingStatusType_UPDATING :: ScalingStatusType
pattern ScalingStatusType_UPDATING = ScalingStatusType' "UPDATING"

{-# COMPLETE
  ScalingStatusType_ACTIVE,
  ScalingStatusType_DELETED,
  ScalingStatusType_DELETE_REQUESTED,
  ScalingStatusType_DELETING,
  ScalingStatusType_ERROR,
  ScalingStatusType_UPDATE_REQUESTED,
  ScalingStatusType_UPDATING,
  ScalingStatusType'
  #-}
