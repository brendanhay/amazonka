{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Method
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Method
  ( Method
      ( Method',
        Get,
        Head,
        Post,
        Put,
        Patch,
        Options,
        Delete
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Method = Method' Lude.Text
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

pattern Get :: Method
pattern Get = Method' "GET"

pattern Head :: Method
pattern Head = Method' "HEAD"

pattern Post :: Method
pattern Post = Method' "POST"

pattern Put :: Method
pattern Put = Method' "PUT"

pattern Patch :: Method
pattern Patch = Method' "PATCH"

pattern Options :: Method
pattern Options = Method' "OPTIONS"

pattern Delete :: Method
pattern Delete = Method' "DELETE"

{-# COMPLETE
  Get,
  Head,
  Post,
  Put,
  Patch,
  Options,
  Delete,
  Method'
  #-}
