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

import qualified Network.AWS.Prelude as Prelude

newtype AssignmentStatusType = AssignmentStatusType'
  { fromAssignmentStatusType ::
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
