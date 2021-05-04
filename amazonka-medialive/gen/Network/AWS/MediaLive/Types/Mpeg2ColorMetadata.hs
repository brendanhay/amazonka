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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
  ( Mpeg2ColorMetadata
      ( ..,
        Mpeg2ColorMetadata_IGNORE,
        Mpeg2ColorMetadata_INSERT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Mpeg2 Color Metadata
newtype Mpeg2ColorMetadata = Mpeg2ColorMetadata'
  { fromMpeg2ColorMetadata ::
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

pattern Mpeg2ColorMetadata_IGNORE :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadata_IGNORE = Mpeg2ColorMetadata' "IGNORE"

pattern Mpeg2ColorMetadata_INSERT :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadata_INSERT = Mpeg2ColorMetadata' "INSERT"

{-# COMPLETE
  Mpeg2ColorMetadata_IGNORE,
  Mpeg2ColorMetadata_INSERT,
  Mpeg2ColorMetadata'
  #-}
