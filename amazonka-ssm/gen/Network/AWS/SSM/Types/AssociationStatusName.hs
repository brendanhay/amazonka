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
-- Module      : Network.AWS.SSM.Types.AssociationStatusName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatusName
  ( AssociationStatusName
      ( ..,
        AssociationStatusName_Failed,
        AssociationStatusName_Pending,
        AssociationStatusName_Success
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AssociationStatusName = AssociationStatusName'
  { fromAssociationStatusName ::
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

pattern AssociationStatusName_Failed :: AssociationStatusName
pattern AssociationStatusName_Failed = AssociationStatusName' "Failed"

pattern AssociationStatusName_Pending :: AssociationStatusName
pattern AssociationStatusName_Pending = AssociationStatusName' "Pending"

pattern AssociationStatusName_Success :: AssociationStatusName
pattern AssociationStatusName_Success = AssociationStatusName' "Success"

{-# COMPLETE
  AssociationStatusName_Failed,
  AssociationStatusName_Pending,
  AssociationStatusName_Success,
  AssociationStatusName'
  #-}
