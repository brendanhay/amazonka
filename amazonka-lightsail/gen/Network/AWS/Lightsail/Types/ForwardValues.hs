{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ForwardValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ForwardValues
  ( ForwardValues
      ( ..,
        ForwardValues_All,
        ForwardValues_Allow_list,
        ForwardValues_None
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ForwardValues = ForwardValues'
  { fromForwardValues ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ForwardValues_All :: ForwardValues
pattern ForwardValues_All = ForwardValues' "all"

pattern ForwardValues_Allow_list :: ForwardValues
pattern ForwardValues_Allow_list = ForwardValues' "allow-list"

pattern ForwardValues_None :: ForwardValues
pattern ForwardValues_None = ForwardValues' "none"

{-# COMPLETE
  ForwardValues_All,
  ForwardValues_Allow_list,
  ForwardValues_None,
  ForwardValues'
  #-}
