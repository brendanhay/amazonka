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
-- Module      : Amazonka.MediaLive.Types.VideoDescriptionRespondToAfd
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoDescriptionRespondToAfd
  ( VideoDescriptionRespondToAfd
      ( ..,
        VideoDescriptionRespondToAfd_NONE,
        VideoDescriptionRespondToAfd_PASSTHROUGH,
        VideoDescriptionRespondToAfd_RESPOND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Video Description Respond To Afd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd'
  { fromVideoDescriptionRespondToAfd ::
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
