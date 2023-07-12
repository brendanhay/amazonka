{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMailMessageFlow.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMailMessageFlow.Lens
  ( -- * Operations

    -- ** GetRawMessageContent
    getRawMessageContent_messageId,
    getRawMessageContentResponse_httpStatus,
    getRawMessageContentResponse_messageContent,

    -- ** PutRawMessageContent
    putRawMessageContent_messageId,
    putRawMessageContent_content,
    putRawMessageContentResponse_httpStatus,

    -- * Types

    -- ** RawMessageContent
    rawMessageContent_s3Reference,

    -- ** S3Reference
    s3Reference_objectVersion,
    s3Reference_bucket,
    s3Reference_key,
  )
where

import Amazonka.WorkMailMessageFlow.GetRawMessageContent
import Amazonka.WorkMailMessageFlow.PutRawMessageContent
import Amazonka.WorkMailMessageFlow.Types.RawMessageContent
import Amazonka.WorkMailMessageFlow.Types.S3Reference
