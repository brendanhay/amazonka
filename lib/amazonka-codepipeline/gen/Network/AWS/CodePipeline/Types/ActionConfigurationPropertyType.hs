{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
  ( ActionConfigurationPropertyType
      ( ActionConfigurationPropertyType',
        String,
        Number,
        Boolean
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType' Lude.Text
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

pattern String :: ActionConfigurationPropertyType
pattern String = ActionConfigurationPropertyType' "String"

pattern Number :: ActionConfigurationPropertyType
pattern Number = ActionConfigurationPropertyType' "Number"

pattern Boolean :: ActionConfigurationPropertyType
pattern Boolean = ActionConfigurationPropertyType' "Boolean"

{-# COMPLETE
  String,
  Number,
  Boolean,
  ActionConfigurationPropertyType'
  #-}
