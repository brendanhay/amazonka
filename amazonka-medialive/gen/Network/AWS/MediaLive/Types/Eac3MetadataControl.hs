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
-- Module      : Network.AWS.MediaLive.Types.Eac3MetadataControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3MetadataControl
  ( Eac3MetadataControl
      ( ..,
        Eac3MetadataControl_FOLLOW_INPUT,
        Eac3MetadataControl_USE_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Metadata Control
newtype Eac3MetadataControl = Eac3MetadataControl'
  { fromEac3MetadataControl ::
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

pattern Eac3MetadataControl_FOLLOW_INPUT :: Eac3MetadataControl
pattern Eac3MetadataControl_FOLLOW_INPUT = Eac3MetadataControl' "FOLLOW_INPUT"

pattern Eac3MetadataControl_USE_CONFIGURED :: Eac3MetadataControl
pattern Eac3MetadataControl_USE_CONFIGURED = Eac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  Eac3MetadataControl_FOLLOW_INPUT,
  Eac3MetadataControl_USE_CONFIGURED,
  Eac3MetadataControl'
  #-}
