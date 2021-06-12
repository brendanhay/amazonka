{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeEventTopics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about which SNS topics receive status messages from
-- the specified directory.
--
-- If no input parameters are provided, such as DirectoryId or TopicName,
-- this request describes all of the associations in the account.
module Network.AWS.DirectoryService.DescribeEventTopics
  ( -- * Creating a Request
    DescribeEventTopics (..),
    newDescribeEventTopics,

    -- * Request Lenses
    describeEventTopics_directoryId,
    describeEventTopics_topicNames,

    -- * Destructuring the Response
    DescribeEventTopicsResponse (..),
    newDescribeEventTopicsResponse,

    -- * Response Lenses
    describeEventTopicsResponse_eventTopics,
    describeEventTopicsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes event topics.
--
-- /See:/ 'newDescribeEventTopics' smart constructor.
data DescribeEventTopics = DescribeEventTopics'
  { -- | The Directory ID for which to get the list of associated SNS topics. If
    -- this member is null, associations for all Directory IDs are returned.
    directoryId :: Core.Maybe Core.Text,
    -- | A list of SNS topic names for which to obtain the information. If this
    -- member is null, all associations for the specified Directory ID are
    -- returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    topicNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventTopics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'describeEventTopics_directoryId' - The Directory ID for which to get the list of associated SNS topics. If
-- this member is null, associations for all Directory IDs are returned.
--
-- 'topicNames', 'describeEventTopics_topicNames' - A list of SNS topic names for which to obtain the information. If this
-- member is null, all associations for the specified Directory ID are
-- returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
newDescribeEventTopics ::
  DescribeEventTopics
newDescribeEventTopics =
  DescribeEventTopics'
    { directoryId = Core.Nothing,
      topicNames = Core.Nothing
    }

-- | The Directory ID for which to get the list of associated SNS topics. If
-- this member is null, associations for all Directory IDs are returned.
describeEventTopics_directoryId :: Lens.Lens' DescribeEventTopics (Core.Maybe Core.Text)
describeEventTopics_directoryId = Lens.lens (\DescribeEventTopics' {directoryId} -> directoryId) (\s@DescribeEventTopics' {} a -> s {directoryId = a} :: DescribeEventTopics)

-- | A list of SNS topic names for which to obtain the information. If this
-- member is null, all associations for the specified Directory ID are
-- returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeEventTopics_topicNames :: Lens.Lens' DescribeEventTopics (Core.Maybe [Core.Text])
describeEventTopics_topicNames = Lens.lens (\DescribeEventTopics' {topicNames} -> topicNames) (\s@DescribeEventTopics' {} a -> s {topicNames = a} :: DescribeEventTopics) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeEventTopics where
  type
    AWSResponse DescribeEventTopics =
      DescribeEventTopicsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventTopicsResponse'
            Core.<$> (x Core..?> "EventTopics" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventTopics

instance Core.NFData DescribeEventTopics

instance Core.ToHeaders DescribeEventTopics where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeEventTopics" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventTopics where
  toJSON DescribeEventTopics' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DirectoryId" Core..=) Core.<$> directoryId,
            ("TopicNames" Core..=) Core.<$> topicNames
          ]
      )

instance Core.ToPath DescribeEventTopics where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventTopics where
  toQuery = Core.const Core.mempty

-- | The result of a DescribeEventTopic request.
--
-- /See:/ 'newDescribeEventTopicsResponse' smart constructor.
data DescribeEventTopicsResponse = DescribeEventTopicsResponse'
  { -- | A list of SNS topic names that receive status messages from the
    -- specified Directory ID.
    eventTopics :: Core.Maybe [EventTopic],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventTopicsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTopics', 'describeEventTopicsResponse_eventTopics' - A list of SNS topic names that receive status messages from the
-- specified Directory ID.
--
-- 'httpStatus', 'describeEventTopicsResponse_httpStatus' - The response's http status code.
newDescribeEventTopicsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventTopicsResponse
newDescribeEventTopicsResponse pHttpStatus_ =
  DescribeEventTopicsResponse'
    { eventTopics =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of SNS topic names that receive status messages from the
-- specified Directory ID.
describeEventTopicsResponse_eventTopics :: Lens.Lens' DescribeEventTopicsResponse (Core.Maybe [EventTopic])
describeEventTopicsResponse_eventTopics = Lens.lens (\DescribeEventTopicsResponse' {eventTopics} -> eventTopics) (\s@DescribeEventTopicsResponse' {} a -> s {eventTopics = a} :: DescribeEventTopicsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventTopicsResponse_httpStatus :: Lens.Lens' DescribeEventTopicsResponse Core.Int
describeEventTopicsResponse_httpStatus = Lens.lens (\DescribeEventTopicsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventTopicsResponse' {} a -> s {httpStatus = a} :: DescribeEventTopicsResponse)

instance Core.NFData DescribeEventTopicsResponse
