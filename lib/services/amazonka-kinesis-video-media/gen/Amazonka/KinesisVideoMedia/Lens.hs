{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoMedia.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoMedia.Lens
  ( -- * Operations

    -- ** GetMedia
    getMedia_streamARN,
    getMedia_streamName,
    getMedia_startSelector,
    getMediaResponse_contentType,
    getMediaResponse_httpStatus,
    getMediaResponse_payload,

    -- * Types

    -- ** StartSelector
    startSelector_afterFragmentNumber,
    startSelector_continuationToken,
    startSelector_startTimestamp,
    startSelector_startSelectorType,
  )
where

import Amazonka.KinesisVideoMedia.GetMedia
import Amazonka.KinesisVideoMedia.Types.StartSelector
