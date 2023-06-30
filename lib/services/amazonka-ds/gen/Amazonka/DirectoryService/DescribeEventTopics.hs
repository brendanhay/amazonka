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
-- Module      : Amazonka.DirectoryService.DescribeEventTopics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about which Amazon SNS topics receive status
-- messages from the specified directory.
--
-- If no input parameters are provided, such as DirectoryId or TopicName,
-- this request describes all of the associations in the account.
module Amazonka.DirectoryService.DescribeEventTopics
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Describes event topics.
--
-- /See:/ 'newDescribeEventTopics' smart constructor.
data DescribeEventTopics = DescribeEventTopics'
  { -- | The Directory ID for which to get the list of associated Amazon SNS
    -- topics. If this member is null, associations for all Directory IDs are
    -- returned.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon SNS topic names for which to obtain the information. If
    -- this member is null, all associations for the specified Directory ID are
    -- returned.
    --
    -- An empty list results in an @InvalidParameterException@ being thrown.
    topicNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventTopics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'describeEventTopics_directoryId' - The Directory ID for which to get the list of associated Amazon SNS
-- topics. If this member is null, associations for all Directory IDs are
-- returned.
--
-- 'topicNames', 'describeEventTopics_topicNames' - A list of Amazon SNS topic names for which to obtain the information. If
-- this member is null, all associations for the specified Directory ID are
-- returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
newDescribeEventTopics ::
  DescribeEventTopics
newDescribeEventTopics =
  DescribeEventTopics'
    { directoryId = Prelude.Nothing,
      topicNames = Prelude.Nothing
    }

-- | The Directory ID for which to get the list of associated Amazon SNS
-- topics. If this member is null, associations for all Directory IDs are
-- returned.
describeEventTopics_directoryId :: Lens.Lens' DescribeEventTopics (Prelude.Maybe Prelude.Text)
describeEventTopics_directoryId = Lens.lens (\DescribeEventTopics' {directoryId} -> directoryId) (\s@DescribeEventTopics' {} a -> s {directoryId = a} :: DescribeEventTopics)

-- | A list of Amazon SNS topic names for which to obtain the information. If
-- this member is null, all associations for the specified Directory ID are
-- returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
describeEventTopics_topicNames :: Lens.Lens' DescribeEventTopics (Prelude.Maybe [Prelude.Text])
describeEventTopics_topicNames = Lens.lens (\DescribeEventTopics' {topicNames} -> topicNames) (\s@DescribeEventTopics' {} a -> s {topicNames = a} :: DescribeEventTopics) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeEventTopics where
  type
    AWSResponse DescribeEventTopics =
      DescribeEventTopicsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventTopicsResponse'
            Prelude.<$> (x Data..?> "EventTopics" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventTopics where
  hashWithSalt _salt DescribeEventTopics' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` topicNames

instance Prelude.NFData DescribeEventTopics where
  rnf DescribeEventTopics' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf topicNames

instance Data.ToHeaders DescribeEventTopics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeEventTopics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventTopics where
  toJSON DescribeEventTopics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("TopicNames" Data..=) Prelude.<$> topicNames
          ]
      )

instance Data.ToPath DescribeEventTopics where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventTopics where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DescribeEventTopic request.
--
-- /See:/ 'newDescribeEventTopicsResponse' smart constructor.
data DescribeEventTopicsResponse = DescribeEventTopicsResponse'
  { -- | A list of Amazon SNS topic names that receive status messages from the
    -- specified Directory ID.
    eventTopics :: Prelude.Maybe [EventTopic],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventTopicsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTopics', 'describeEventTopicsResponse_eventTopics' - A list of Amazon SNS topic names that receive status messages from the
-- specified Directory ID.
--
-- 'httpStatus', 'describeEventTopicsResponse_httpStatus' - The response's http status code.
newDescribeEventTopicsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventTopicsResponse
newDescribeEventTopicsResponse pHttpStatus_ =
  DescribeEventTopicsResponse'
    { eventTopics =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Amazon SNS topic names that receive status messages from the
-- specified Directory ID.
describeEventTopicsResponse_eventTopics :: Lens.Lens' DescribeEventTopicsResponse (Prelude.Maybe [EventTopic])
describeEventTopicsResponse_eventTopics = Lens.lens (\DescribeEventTopicsResponse' {eventTopics} -> eventTopics) (\s@DescribeEventTopicsResponse' {} a -> s {eventTopics = a} :: DescribeEventTopicsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventTopicsResponse_httpStatus :: Lens.Lens' DescribeEventTopicsResponse Prelude.Int
describeEventTopicsResponse_httpStatus = Lens.lens (\DescribeEventTopicsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventTopicsResponse' {} a -> s {httpStatus = a} :: DescribeEventTopicsResponse)

instance Prelude.NFData DescribeEventTopicsResponse where
  rnf DescribeEventTopicsResponse' {..} =
    Prelude.rnf eventTopics
      `Prelude.seq` Prelude.rnf httpStatus
