{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
  ( RedirectActionStatusCodeEnum
      ( RedirectActionStatusCodeEnum',
        HTTP301,
        HTTP302
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RedirectActionStatusCodeEnum = RedirectActionStatusCodeEnum' Lude.Text
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

pattern HTTP301 :: RedirectActionStatusCodeEnum
pattern HTTP301 = RedirectActionStatusCodeEnum' "HTTP_301"

pattern HTTP302 :: RedirectActionStatusCodeEnum
pattern HTTP302 = RedirectActionStatusCodeEnum' "HTTP_302"

{-# COMPLETE
  HTTP301,
  HTTP302,
  RedirectActionStatusCodeEnum'
  #-}
