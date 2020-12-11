-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Runtime
  ( Runtime
      ( Runtime',
        DOTNETCORE1_0,
        DOTNETCORE2_0,
        DOTNETCORE2_1,
        DOTNETCORE3_1,
        GO1_x,
        JAVA11,
        JAVA8,
        JAVA8_AL2,
        NODEJS10_x,
        NODEJS12_x,
        NODEJS4_3,
        NODEJS4_3Edge,
        NODEJS6_10,
        NODEJS8_10,
        Nodejs,
        PYTHON2_7,
        PYTHON3_6,
        PYTHON3_7,
        PYTHON3_8,
        Provided,
        Provided_AL2,
        RUBY2_5,
        RUBY2_7
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Runtime = Runtime' Lude.Text
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

pattern DOTNETCORE1_0 :: Runtime
pattern DOTNETCORE1_0 = Runtime' "dotnetcore1.0"

pattern DOTNETCORE2_0 :: Runtime
pattern DOTNETCORE2_0 = Runtime' "dotnetcore2.0"

pattern DOTNETCORE2_1 :: Runtime
pattern DOTNETCORE2_1 = Runtime' "dotnetcore2.1"

pattern DOTNETCORE3_1 :: Runtime
pattern DOTNETCORE3_1 = Runtime' "dotnetcore3.1"

pattern GO1_x :: Runtime
pattern GO1_x = Runtime' "go1.x"

pattern JAVA11 :: Runtime
pattern JAVA11 = Runtime' "java11"

pattern JAVA8 :: Runtime
pattern JAVA8 = Runtime' "java8"

pattern JAVA8_AL2 :: Runtime
pattern JAVA8_AL2 = Runtime' "java8.al2"

pattern NODEJS10_x :: Runtime
pattern NODEJS10_x = Runtime' "nodejs10.x"

pattern NODEJS12_x :: Runtime
pattern NODEJS12_x = Runtime' "nodejs12.x"

pattern NODEJS4_3 :: Runtime
pattern NODEJS4_3 = Runtime' "nodejs4.3"

pattern NODEJS4_3Edge :: Runtime
pattern NODEJS4_3Edge = Runtime' "nodejs4.3-edge"

pattern NODEJS6_10 :: Runtime
pattern NODEJS6_10 = Runtime' "nodejs6.10"

pattern NODEJS8_10 :: Runtime
pattern NODEJS8_10 = Runtime' "nodejs8.10"

pattern Nodejs :: Runtime
pattern Nodejs = Runtime' "nodejs"

pattern PYTHON2_7 :: Runtime
pattern PYTHON2_7 = Runtime' "python2.7"

pattern PYTHON3_6 :: Runtime
pattern PYTHON3_6 = Runtime' "python3.6"

pattern PYTHON3_7 :: Runtime
pattern PYTHON3_7 = Runtime' "python3.7"

pattern PYTHON3_8 :: Runtime
pattern PYTHON3_8 = Runtime' "python3.8"

pattern Provided :: Runtime
pattern Provided = Runtime' "provided"

pattern Provided_AL2 :: Runtime
pattern Provided_AL2 = Runtime' "provided.al2"

pattern RUBY2_5 :: Runtime
pattern RUBY2_5 = Runtime' "ruby2.5"

pattern RUBY2_7 :: Runtime
pattern RUBY2_7 = Runtime' "ruby2.7"

{-# COMPLETE
  DOTNETCORE1_0,
  DOTNETCORE2_0,
  DOTNETCORE2_1,
  DOTNETCORE3_1,
  GO1_x,
  JAVA11,
  JAVA8,
  JAVA8_AL2,
  NODEJS10_x,
  NODEJS12_x,
  NODEJS4_3,
  NODEJS4_3Edge,
  NODEJS6_10,
  NODEJS8_10,
  Nodejs,
  PYTHON2_7,
  PYTHON3_6,
  PYTHON3_7,
  PYTHON3_8,
  Provided,
  Provided_AL2,
  RUBY2_5,
  RUBY2_7,
  Runtime'
  #-}
