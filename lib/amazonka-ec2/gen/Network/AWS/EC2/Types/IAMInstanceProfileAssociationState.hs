-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
  ( IAMInstanceProfileAssociationState
      ( IAMInstanceProfileAssociationState',
        IAPASAssociated,
        IAPASAssociating,
        IAPASDisassociated,
        IAPASDisassociating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IAMInstanceProfileAssociationState = IAMInstanceProfileAssociationState' Lude.Text
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

pattern IAPASAssociated :: IAMInstanceProfileAssociationState
pattern IAPASAssociated = IAMInstanceProfileAssociationState' "associated"

pattern IAPASAssociating :: IAMInstanceProfileAssociationState
pattern IAPASAssociating = IAMInstanceProfileAssociationState' "associating"

pattern IAPASDisassociated :: IAMInstanceProfileAssociationState
pattern IAPASDisassociated = IAMInstanceProfileAssociationState' "disassociated"

pattern IAPASDisassociating :: IAMInstanceProfileAssociationState
pattern IAPASDisassociating = IAMInstanceProfileAssociationState' "disassociating"

{-# COMPLETE
  IAPASAssociated,
  IAPASAssociating,
  IAPASDisassociated,
  IAPASDisassociating,
  IAMInstanceProfileAssociationState'
  #-}
