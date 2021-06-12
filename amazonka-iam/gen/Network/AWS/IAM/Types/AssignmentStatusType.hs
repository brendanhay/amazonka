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
-- Module      : Network.AWS.IAM.Types.AssignmentStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AssignmentStatusType
  ( AssignmentStatusType
      ( ..,
        AssignmentStatusType_Any,
        AssignmentStatusType_Assigned,
        AssignmentStatusType_Unassigned
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AssignmentStatusType = AssignmentStatusType'
  { fromAssignmentStatusType ::
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

pattern AssignmentStatusType_Any :: AssignmentStatusType
pattern AssignmentStatusType_Any = AssignmentStatusType' "Any"

pattern AssignmentStatusType_Assigned :: AssignmentStatusType
pattern AssignmentStatusType_Assigned = AssignmentStatusType' "Assigned"

pattern AssignmentStatusType_Unassigned :: AssignmentStatusType
pattern AssignmentStatusType_Unassigned = AssignmentStatusType' "Unassigned"

{-# COMPLETE
  AssignmentStatusType_Any,
  AssignmentStatusType_Assigned,
  AssignmentStatusType_Unassigned,
  AssignmentStatusType'
  #-}
