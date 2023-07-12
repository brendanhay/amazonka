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
-- Module      : Amazonka.PinpointEmail.Types.DimensionValueSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DimensionValueSource
  ( DimensionValueSource
      ( ..,
        DimensionValueSource_EMAIL_HEADER,
        DimensionValueSource_LINK_TAG,
        DimensionValueSource_MESSAGE_TAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location where Amazon Pinpoint finds the value of a dimension to
-- publish to Amazon CloudWatch. If you want Amazon Pinpoint to use the
-- message tags that you specify using an X-SES-MESSAGE-TAGS header or a
-- parameter to the SendEmail\/SendRawEmail API, choose @messageTag@. If
-- you want Amazon Pinpoint to use your own email headers, choose
-- @emailHeader@. If you want Amazon Pinpoint to use link tags, choose
-- @linkTags@.
newtype DimensionValueSource = DimensionValueSource'
  { fromDimensionValueSource ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
