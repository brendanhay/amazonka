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
-- Module      : Network.AWS.SESv2.Types.DimensionValueSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DimensionValueSource
  ( DimensionValueSource
      ( ..,
        DimensionValueSource_EMAIL_HEADER,
        DimensionValueSource_LINK_TAG,
        DimensionValueSource_MESSAGE_TAG
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The location where the Amazon SES API v2 finds the value of a dimension
-- to publish to Amazon CloudWatch. If you want to use the message tags
-- that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to
-- the @SendEmail@ or @SendRawEmail@ API, choose @messageTag@. If you want
-- to use your own email headers, choose @emailHeader@. If you want to use
-- link tags, choose @linkTags@.
newtype DimensionValueSource = DimensionValueSource'
  { fromDimensionValueSource ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern DimensionValueSource_EMAIL_HEADER :: DimensionValueSource
pattern DimensionValueSource_EMAIL_HEADER = DimensionValueSource' "EMAIL_HEADER"

pattern DimensionValueSource_LINK_TAG :: DimensionValueSource
pattern DimensionValueSource_LINK_TAG = DimensionValueSource' "LINK_TAG"

pattern DimensionValueSource_MESSAGE_TAG :: DimensionValueSource
pattern DimensionValueSource_MESSAGE_TAG = DimensionValueSource' "MESSAGE_TAG"

{-# COMPLETE
  DimensionValueSource_EMAIL_HEADER,
  DimensionValueSource_LINK_TAG,
  DimensionValueSource_MESSAGE_TAG,
  DimensionValueSource'
  #-}
