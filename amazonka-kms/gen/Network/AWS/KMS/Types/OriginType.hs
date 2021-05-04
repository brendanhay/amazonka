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
-- Module      : Network.AWS.KMS.Types.OriginType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.OriginType
  ( OriginType
      ( ..,
        OriginType_AWS_CLOUDHSM,
        OriginType_AWS_KMS,
        OriginType_EXTERNAL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OriginType = OriginType'
  { fromOriginType ::
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

pattern OriginType_AWS_CLOUDHSM :: OriginType
pattern OriginType_AWS_CLOUDHSM = OriginType' "AWS_CLOUDHSM"

pattern OriginType_AWS_KMS :: OriginType
pattern OriginType_AWS_KMS = OriginType' "AWS_KMS"

pattern OriginType_EXTERNAL :: OriginType
pattern OriginType_EXTERNAL = OriginType' "EXTERNAL"

{-# COMPLETE
  OriginType_AWS_CLOUDHSM,
  OriginType_AWS_KMS,
  OriginType_EXTERNAL,
  OriginType'
  #-}
