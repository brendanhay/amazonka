{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Lens
  ( -- * Operations

    -- ** SendAnnouncement
    sendAnnouncement_timeToLiveInSeconds,
    sendAnnouncement_roomFilters,
    sendAnnouncement_content,
    sendAnnouncement_clientRequestToken,
    sendAnnouncementResponse_announcementArn,
    sendAnnouncementResponse_httpStatus,

    -- * Types

    -- ** Audio
    audio_locale,
    audio_location,

    -- ** Content
    content_audioList,
    content_ssmlList,
    content_textList,

    -- ** Filter
    filter_key,
    filter_values,

    -- ** Ssml
    ssml_locale,
    ssml_value,

    -- ** TextMessage
    textMessage_locale,
    textMessage_value,
  )
where

import Amazonka.AlexaBusiness.SendAnnouncement
import Amazonka.AlexaBusiness.Types.Audio
import Amazonka.AlexaBusiness.Types.Content
import Amazonka.AlexaBusiness.Types.Filter
import Amazonka.AlexaBusiness.Types.Ssml
import Amazonka.AlexaBusiness.Types.TextMessage
