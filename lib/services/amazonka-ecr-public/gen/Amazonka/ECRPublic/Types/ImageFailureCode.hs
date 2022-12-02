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
-- Module      : Amazonka.ECRPublic.Types.ImageFailureCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.ImageFailureCode
  ( ImageFailureCode
      ( ..,
        ImageFailureCode_ImageNotFound,
        ImageFailureCode_ImageReferencedByManifestList,
        ImageFailureCode_ImageTagDoesNotMatchDigest,
        ImageFailureCode_InvalidImageDigest,
        ImageFailureCode_InvalidImageTag,
        ImageFailureCode_KmsError,
        ImageFailureCode_MissingDigestAndTag
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageFailureCode = ImageFailureCode'
  { fromImageFailureCode ::
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

pattern ImageFailureCode_ImageNotFound :: ImageFailureCode
pattern ImageFailureCode_ImageNotFound = ImageFailureCode' "ImageNotFound"

pattern ImageFailureCode_ImageReferencedByManifestList :: ImageFailureCode
pattern ImageFailureCode_ImageReferencedByManifestList = ImageFailureCode' "ImageReferencedByManifestList"

pattern ImageFailureCode_ImageTagDoesNotMatchDigest :: ImageFailureCode
pattern ImageFailureCode_ImageTagDoesNotMatchDigest = ImageFailureCode' "ImageTagDoesNotMatchDigest"

pattern ImageFailureCode_InvalidImageDigest :: ImageFailureCode
pattern ImageFailureCode_InvalidImageDigest = ImageFailureCode' "InvalidImageDigest"

pattern ImageFailureCode_InvalidImageTag :: ImageFailureCode
pattern ImageFailureCode_InvalidImageTag = ImageFailureCode' "InvalidImageTag"

pattern ImageFailureCode_KmsError :: ImageFailureCode
pattern ImageFailureCode_KmsError = ImageFailureCode' "KmsError"

pattern ImageFailureCode_MissingDigestAndTag :: ImageFailureCode
pattern ImageFailureCode_MissingDigestAndTag = ImageFailureCode' "MissingDigestAndTag"

{-# COMPLETE
  ImageFailureCode_ImageNotFound,
  ImageFailureCode_ImageReferencedByManifestList,
  ImageFailureCode_ImageTagDoesNotMatchDigest,
  ImageFailureCode_InvalidImageDigest,
  ImageFailureCode_InvalidImageTag,
  ImageFailureCode_KmsError,
  ImageFailureCode_MissingDigestAndTag,
  ImageFailureCode'
  #-}
