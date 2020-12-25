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
        ChangeSourceResourceReference,
        ChangeSourceParameterReference,
        ChangeSourceResourceAttribute,
        ChangeSourceDirectModification,
        ChangeSourceAutomatic,
        fromChangeSource
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChangeSource = ChangeSource' {fromChangeSource :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ChangeSourceResourceReference :: ChangeSource
pattern ChangeSourceResourceReference = ChangeSource' "ResourceReference"

pattern ChangeSourceParameterReference :: ChangeSource
pattern ChangeSourceParameterReference = ChangeSource' "ParameterReference"

pattern ChangeSourceResourceAttribute :: ChangeSource
pattern ChangeSourceResourceAttribute = ChangeSource' "ResourceAttribute"

pattern ChangeSourceDirectModification :: ChangeSource
pattern ChangeSourceDirectModification = ChangeSource' "DirectModification"

pattern ChangeSourceAutomatic :: ChangeSource
pattern ChangeSourceAutomatic = ChangeSource' "Automatic"

{-# COMPLETE
  ChangeSourceResourceReference,
  ChangeSourceParameterReference,
  ChangeSourceResourceAttribute,
  ChangeSourceDirectModification,
  ChangeSourceAutomatic,
  ChangeSource'
  #-}
