{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatusName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatusName
  ( AssociationStatusName
      ( AssociationStatusName',
        Pending,
        Success,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssociationStatusName = AssociationStatusName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Pending :: AssociationStatusName
pattern Pending = AssociationStatusName' "Pending"

pattern Success :: AssociationStatusName
pattern Success = AssociationStatusName' "Success"

pattern Failed :: AssociationStatusName
pattern Failed = AssociationStatusName' "Failed"

{-# COMPLETE
  Pending,
  Success,
  Failed,
  AssociationStatusName'
  #-}
