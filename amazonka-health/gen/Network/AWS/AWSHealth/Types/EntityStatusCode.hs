{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityStatusCode
  ( EntityStatusCode
      ( ..,
        EntityStatusCode_IMPAIRED,
        EntityStatusCode_UNIMPAIRED,
        EntityStatusCode_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EntityStatusCode = EntityStatusCode'
  { fromEntityStatusCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern EntityStatusCode_IMPAIRED :: EntityStatusCode
pattern EntityStatusCode_IMPAIRED = EntityStatusCode' "IMPAIRED"

pattern EntityStatusCode_UNIMPAIRED :: EntityStatusCode
pattern EntityStatusCode_UNIMPAIRED = EntityStatusCode' "UNIMPAIRED"

pattern EntityStatusCode_UNKNOWN :: EntityStatusCode
pattern EntityStatusCode_UNKNOWN = EntityStatusCode' "UNKNOWN"

{-# COMPLETE
  EntityStatusCode_IMPAIRED,
  EntityStatusCode_UNIMPAIRED,
  EntityStatusCode_UNKNOWN,
  EntityStatusCode'
  #-}
