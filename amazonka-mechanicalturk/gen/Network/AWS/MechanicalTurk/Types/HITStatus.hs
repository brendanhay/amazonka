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
-- Module      : Network.AWS.MechanicalTurk.Types.HITStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITStatus
  ( HITStatus
      ( ..,
        HITStatus_Assignable,
        HITStatus_Disposed,
        HITStatus_Reviewable,
        HITStatus_Reviewing,
        HITStatus_Unassignable
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HITStatus = HITStatus'
  { fromHITStatus ::
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

pattern HITStatus_Assignable :: HITStatus
pattern HITStatus_Assignable = HITStatus' "Assignable"

pattern HITStatus_Disposed :: HITStatus
pattern HITStatus_Disposed = HITStatus' "Disposed"

pattern HITStatus_Reviewable :: HITStatus
pattern HITStatus_Reviewable = HITStatus' "Reviewable"

pattern HITStatus_Reviewing :: HITStatus
pattern HITStatus_Reviewing = HITStatus' "Reviewing"

pattern HITStatus_Unassignable :: HITStatus
pattern HITStatus_Unassignable = HITStatus' "Unassignable"

{-# COMPLETE
  HITStatus_Assignable,
  HITStatus_Disposed,
  HITStatus_Reviewable,
  HITStatus_Reviewing,
  HITStatus_Unassignable,
  HITStatus'
  #-}
