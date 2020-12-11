-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilterName
  ( GroupFilterName
      ( GroupFilterName',
        GFNConfigurationType,
        GFNResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GroupFilterName = GroupFilterName' Lude.Text
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

pattern GFNConfigurationType :: GroupFilterName
pattern GFNConfigurationType = GroupFilterName' "configuration-type"

pattern GFNResourceType :: GroupFilterName
pattern GFNResourceType = GroupFilterName' "resource-type"

{-# COMPLETE
  GFNConfigurationType,
  GFNResourceType,
  GroupFilterName'
  #-}
