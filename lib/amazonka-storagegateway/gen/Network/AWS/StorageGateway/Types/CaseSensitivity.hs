{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CaseSensitivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CaseSensitivity
  ( CaseSensitivity
      ( CaseSensitivity',
        CaseSensitive,
        ClientSpecified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CaseSensitivity = CaseSensitivity' Lude.Text
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

pattern CaseSensitive :: CaseSensitivity
pattern CaseSensitive = CaseSensitivity' "CaseSensitive"

pattern ClientSpecified :: CaseSensitivity
pattern ClientSpecified = CaseSensitivity' "ClientSpecified"

{-# COMPLETE
  CaseSensitive,
  ClientSpecified,
  CaseSensitivity'
  #-}
