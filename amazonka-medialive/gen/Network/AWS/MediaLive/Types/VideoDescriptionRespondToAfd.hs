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
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
  ( VideoDescriptionRespondToAfd
      ( ..,
        VideoDescriptionRespondToAfd_NONE,
        VideoDescriptionRespondToAfd_PASSTHROUGH,
        VideoDescriptionRespondToAfd_RESPOND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Video Description Respond To Afd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd'
  { fromVideoDescriptionRespondToAfd ::
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

pattern VideoDescriptionRespondToAfd_NONE :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfd_NONE = VideoDescriptionRespondToAfd' "NONE"

pattern VideoDescriptionRespondToAfd_PASSTHROUGH :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfd_PASSTHROUGH = VideoDescriptionRespondToAfd' "PASSTHROUGH"

pattern VideoDescriptionRespondToAfd_RESPOND :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfd_RESPOND = VideoDescriptionRespondToAfd' "RESPOND"

{-# COMPLETE
  VideoDescriptionRespondToAfd_NONE,
  VideoDescriptionRespondToAfd_PASSTHROUGH,
  VideoDescriptionRespondToAfd_RESPOND,
  VideoDescriptionRespondToAfd'
  #-}
