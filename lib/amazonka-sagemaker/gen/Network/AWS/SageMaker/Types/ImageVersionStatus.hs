{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionStatus
  ( ImageVersionStatus
      ( ImageVersionStatus',
        IVSCreating,
        IVSCreated,
        IVSCreateFailed,
        IVSDeleting,
        IVSDeleteFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageVersionStatus = ImageVersionStatus' Lude.Text
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

pattern IVSCreating :: ImageVersionStatus
pattern IVSCreating = ImageVersionStatus' "CREATING"

pattern IVSCreated :: ImageVersionStatus
pattern IVSCreated = ImageVersionStatus' "CREATED"

pattern IVSCreateFailed :: ImageVersionStatus
pattern IVSCreateFailed = ImageVersionStatus' "CREATE_FAILED"

pattern IVSDeleting :: ImageVersionStatus
pattern IVSDeleting = ImageVersionStatus' "DELETING"

pattern IVSDeleteFailed :: ImageVersionStatus
pattern IVSDeleteFailed = ImageVersionStatus' "DELETE_FAILED"

{-# COMPLETE
  IVSCreating,
  IVSCreated,
  IVSCreateFailed,
  IVSDeleting,
  IVSDeleteFailed,
  ImageVersionStatus'
  #-}
