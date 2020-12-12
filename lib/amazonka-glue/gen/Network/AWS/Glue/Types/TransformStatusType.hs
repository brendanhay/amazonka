{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformStatusType
  ( TransformStatusType
      ( TransformStatusType',
        Deleting,
        NotReady,
        Ready
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransformStatusType = TransformStatusType' Lude.Text
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

pattern Deleting :: TransformStatusType
pattern Deleting = TransformStatusType' "DELETING"

pattern NotReady :: TransformStatusType
pattern NotReady = TransformStatusType' "NOT_READY"

pattern Ready :: TransformStatusType
pattern Ready = TransformStatusType' "READY"

{-# COMPLETE
  Deleting,
  NotReady,
  Ready,
  TransformStatusType'
  #-}
