{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
  ( RefreshSchemasStatusTypeValue
      ( RefreshSchemasStatusTypeValue',
        Successful,
        Failed,
        Refreshing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RefreshSchemasStatusTypeValue = RefreshSchemasStatusTypeValue' Lude.Text
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

pattern Successful :: RefreshSchemasStatusTypeValue
pattern Successful = RefreshSchemasStatusTypeValue' "successful"

pattern Failed :: RefreshSchemasStatusTypeValue
pattern Failed = RefreshSchemasStatusTypeValue' "failed"

pattern Refreshing :: RefreshSchemasStatusTypeValue
pattern Refreshing = RefreshSchemasStatusTypeValue' "refreshing"

{-# COMPLETE
  Successful,
  Failed,
  Refreshing,
  RefreshSchemasStatusTypeValue'
  #-}
