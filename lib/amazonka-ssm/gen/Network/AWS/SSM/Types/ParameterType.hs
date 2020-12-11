-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterType
  ( ParameterType
      ( ParameterType',
        PTSecureString,
        PTString,
        PTStringList
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ParameterType = ParameterType' Lude.Text
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

pattern PTSecureString :: ParameterType
pattern PTSecureString = ParameterType' "SecureString"

pattern PTString :: ParameterType
pattern PTString = ParameterType' "String"

pattern PTStringList :: ParameterType
pattern PTStringList = ParameterType' "StringList"

{-# COMPLETE
  PTSecureString,
  PTString,
  PTStringList,
  ParameterType'
  #-}
