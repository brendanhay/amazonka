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
-- Module      : Amazonka.Signer.Types.ImageFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.ImageFormat
  ( ImageFormat
      ( ..,
        ImageFormat_JSON,
        ImageFormat_JSONDetached,
        ImageFormat_JSONEmbedded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageFormat = ImageFormat'
  { fromImageFormat ::
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

pattern ImageFormat_JSON :: ImageFormat
pattern ImageFormat_JSON = ImageFormat' "JSON"

pattern ImageFormat_JSONDetached :: ImageFormat
pattern ImageFormat_JSONDetached = ImageFormat' "JSONDetached"

pattern ImageFormat_JSONEmbedded :: ImageFormat
pattern ImageFormat_JSONEmbedded = ImageFormat' "JSONEmbedded"

{-# COMPLETE
  ImageFormat_JSON,
  ImageFormat_JSONDetached,
  ImageFormat_JSONEmbedded,
  ImageFormat'
  #-}
