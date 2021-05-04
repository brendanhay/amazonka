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
-- Module      : Network.AWS.DirectoryService.Types.RadiusStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusStatus
  ( RadiusStatus
      ( ..,
        RadiusStatus_Completed,
        RadiusStatus_Creating,
        RadiusStatus_Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RadiusStatus = RadiusStatus'
  { fromRadiusStatus ::
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

pattern RadiusStatus_Completed :: RadiusStatus
pattern RadiusStatus_Completed = RadiusStatus' "Completed"

pattern RadiusStatus_Creating :: RadiusStatus
pattern RadiusStatus_Creating = RadiusStatus' "Creating"

pattern RadiusStatus_Failed :: RadiusStatus
pattern RadiusStatus_Failed = RadiusStatus' "Failed"

{-# COMPLETE
  RadiusStatus_Completed,
  RadiusStatus_Creating,
  RadiusStatus_Failed,
  RadiusStatus'
  #-}
