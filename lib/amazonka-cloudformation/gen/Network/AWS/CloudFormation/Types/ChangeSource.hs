{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSource
  ( ChangeSource
      ( ChangeSource',
        Automatic,
        DirectModification,
        ParameterReference,
        ResourceAttribute,
        ResourceReference
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChangeSource = ChangeSource' Lude.Text
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

pattern Automatic :: ChangeSource
pattern Automatic = ChangeSource' "Automatic"

pattern DirectModification :: ChangeSource
pattern DirectModification = ChangeSource' "DirectModification"

pattern ParameterReference :: ChangeSource
pattern ParameterReference = ChangeSource' "ParameterReference"

pattern ResourceAttribute :: ChangeSource
pattern ResourceAttribute = ChangeSource' "ResourceAttribute"

pattern ResourceReference :: ChangeSource
pattern ResourceReference = ChangeSource' "ResourceReference"

{-# COMPLETE
  Automatic,
  DirectModification,
  ParameterReference,
  ResourceAttribute,
  ResourceReference,
  ChangeSource'
  #-}
