-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityStatusCode
  ( EntityStatusCode
      ( EntityStatusCode',
        Impaired,
        Unimpaired,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EntityStatusCode = EntityStatusCode' Lude.Text
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

pattern Impaired :: EntityStatusCode
pattern Impaired = EntityStatusCode' "IMPAIRED"

pattern Unimpaired :: EntityStatusCode
pattern Unimpaired = EntityStatusCode' "UNIMPAIRED"

pattern Unknown :: EntityStatusCode
pattern Unknown = EntityStatusCode' "UNKNOWN"

{-# COMPLETE
  Impaired,
  Unimpaired,
  Unknown,
  EntityStatusCode'
  #-}
