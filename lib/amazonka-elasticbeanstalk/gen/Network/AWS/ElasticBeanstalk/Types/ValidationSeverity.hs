-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
  ( ValidationSeverity
      ( ValidationSeverity',
        Error,
        Warning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ValidationSeverity = ValidationSeverity' Lude.Text
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

pattern Error :: ValidationSeverity
pattern Error = ValidationSeverity' "error"

pattern Warning :: ValidationSeverity
pattern Warning = ValidationSeverity' "warning"

{-# COMPLETE
  Error,
  Warning,
  ValidationSeverity'
  #-}
